port module Main exposing (..)
import Browser exposing (Document)
import Browser.Events
import Browser.Navigation
import Ease
import Graph exposing (..)
import Html exposing (Html, div, button)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
import Random
import Svg exposing (Svg, svg, g)
import Svg.Attributes as SA
import Svg.Events as SE
import Time exposing (Posix, posixToMillis)
import Tuple exposing (pair, first, second)
import Url exposing (Url)
import Util exposing (..)
import Vector as V exposing (Vector)

port saveLocalStorage : E.Value -> Cmd msg

main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subs
    , onUrlRequest = ChangeUrlRequest
    , onUrlChange = UrlChanging
    }

type alias ArmMatch =
    { piece : Piece
    , piece_index : Int
    , arm : Float
    , arm_index: Int
    }

type alias Level =
    { graph : Graph
    , pieces : List Piece
    , n : Int
    , arm_matches : List (List (Maybe ArmMatch))
    }

type AnimationState
    = Interactive
    | ChangingLevel Level
    | MovingPieces (List (Piece, Piece)) Posix

type MoveMode
    = DragAll
    | SelectOne

type alias Model =
    { current_level : Level
    , previous_levels : List Level
    , next_levels : List Level
    , animation_state : AnimationState
    , presses : List Press
    , mouse : Vector
    , time : Posix
    , selected_piece: Maybe (Int,Piece)
    , show_hint: Bool
    }

type alias Piece =
    { position : Point
    , arms : List Float
    }

-- touch: ID of touch, x, y
type TouchInfo = TouchInfo Int Float Float

type PressSource
    = Mouse Int
    | Touch Int

type PressAction
    = Background
    | DragPiece (Int, Piece)

type  PressKind
    = StaticPress
    | MovingPress

type alias Press =
    { start_position: Vector
    , position : Vector
    , source : PressSource
    , action : PressAction
    , kind : PressKind
    }

type Msg
    = MouseMove Float Float
    | MouseDown Int
    | MouseUp Int
    | TouchStart (List TouchInfo)
    | TouchMove (List TouchInfo)
    | TouchEnd (List TouchInfo)
    | KeyDown String
    | KeyPressed String
    | ChangeUrlRequest Browser.UrlRequest
    | UrlChanging Url
    | SetGraph Graph
    | NextLevel
    | PreviousLevel
    | NewRandomGraph
    | ToggleHint
    | Reset
    | Frame Posix
    | Noop

press_buffer : PressSource -> Float
press_buffer source = case source of
    Mouse _ -> 1
    Touch _ -> 3

start_touches : List TouchInfo -> Model -> Model
start_touches touches model =
    List.foldl (\touch -> \m ->
        case touch of
            TouchInfo id x y -> start_press (x,y) (Touch id) m
    ) model touches

start_press : Vector -> PressSource -> Model -> Model
start_press position source model = 
    let
        picked_piece = pick_piece (press_buffer source) position model.current_level.pieces
        action = case model.selected_piece of
            Nothing -> case picked_piece of
                Just (i,p) -> DragPiece (i,p)
                Nothing -> Background
            Just (i,p) -> case listGet i model.current_level.pieces of
                Just p2 -> DragPiece (i,p2)
                Nothing -> DragPiece (i,p)
        press = 
            { start_position = position
            , position = position
            , source = source
            , action = action
            , kind = StaticPress
            }
    in
        { model | presses = press::model.presses}

distance_from_piece pos piece = 
    let
        d : Float
        d = V.len (V.sub pos piece.position)
    in
        max 0 (d - 1)

pick_piece buffer pos = (List.indexedMap (\i -> \p -> ((i,p), distance_from_piece pos p))) >> List.filter (\(_,d) -> d <= buffer) >> List.sortBy second >> List.map first >> List.head

end_touches : List TouchInfo -> Model -> Model
end_touches touches model = List.foldl (\touch -> \m -> case touch of
    TouchInfo id x y -> end_press (Touch id) m
    ) model touches

end_press : PressSource -> Model -> Model
end_press source = (select_piece source >> remove_press source)

select_piece source = press_fold (\press -> \model ->
    case press.kind of
        MovingPress -> model
        StaticPress -> 
            let
                mp1 = pick_piece (press_buffer source) press.start_position model.current_level.pieces
                mp2 = pick_piece (press_buffer source) press.position model.current_level.pieces
                d = V.len (V.sub press.start_position press.position)
            in
                case (mp1,mp2) of
                    (Just (i1,p1), Just (i2,p2)) -> 
                        if p1==p2 then 
                            case model.selected_piece of
                                Just (i3,p3) -> if i1==i3 then { model | selected_piece = Nothing } else { model | selected_piece = Just (i1,p1) }
                                Nothing -> { model | selected_piece = Just (i1,p1) } 
                        else 
                            model
                    _ -> if d < 0.1 then { model | selected_piece = Nothing } else model
    )

remove_press : PressSource -> Model -> Model
remove_press source model = { model | presses = List.filter (\s -> s.source /= source) model.presses }

keydown = D.map KeyDown (D.field "key" D.string)

is_mouse_press source = case source of
    Mouse _ -> True
    _ -> False

decode_mouse : (Int -> Msg) -> D.Decoder Msg
decode_mouse msg = D.map msg (D.field "button" D.int)

decode_event_vector : (Float -> Float -> a) -> D.Decoder a
decode_event_vector fn = 
    D.map2 fn
        (D.at ["detail", "x"] D.float)
        (D.at ["detail", "y"] D.float)

decode_vector : D.Decoder Vector
decode_vector = D.map2 pair (D.field "x" D.float) (D.field "y" D.float)

decode_keypress = D.map KeyPressed <| D.field "key" D.string

decode_touch msg =
    D.field "detail" <| D.map msg <| D.list (D.map3 TouchInfo
        (D.field "identifier" D.int)
        (D.at ["position","x"] D.float)
        (D.at ["position","y"] D.float)
    )

make_level model = make_random_graph model.current_level.n

make_random_graph num_points = Random.generate SetGraph (Graph.random_planar_graph num_points)

make_pieces : Graph -> List Piece
make_pieces g =
    let
        get_linked_points i = 
            List.filterMap 
                (\(j,k) -> 
                    if i==j then Just k
                    else if i==k then Just j
                    else Nothing
                ) 
                g.edges
        make_piece i p =
            let
                linked_points = List.filterMap (\j -> listGet j g.points) (get_linked_points i)
                angles = List.map 
                    (\p2 ->
                        let
                            (dx,dy) = V.sub p2 p
                        in
                            atan2 dy dx
                    )
                    linked_points
            in
                { position = p
                , arms = angles
                }
    in
        List.indexedMap make_piece g.points

nocmd model = (model, Cmd.none)

update msg model = case msg of
    SetGraph g -> set_graph (smoosh_graph g) model |> nocmd |> save
    Reset -> set_graph model.current_level.graph model |> nocmd |> save
    NextLevel ->  next_level model |> save
    PreviousLevel ->  previous_level model |> save
    NewRandomGraph -> new_graph model
    MouseDown button -> (start_press model.mouse (Mouse button) model, Cmd.none)
    MouseUp button -> end_press (Mouse button) model |> nocmd |> save
    MouseMove x y -> (set_mouse (x,y) >> set_press_position is_mouse_press (x,y) >> after_move) model |> nocmd
    TouchStart touches -> start_touches touches model |> nocmd
    TouchMove touches -> (move_touches touches >> after_move) model |> nocmd
    TouchEnd touches -> end_touches touches model|> nocmd |> save
    ToggleHint -> { model | show_hint = not model.show_hint } |> nocmd
    Frame t -> frame t model |> nocmd
    _ -> nocmd model

frame t = set_time t >> update_animation

set_time t model = { model | time = t }

animation_duration = 1000

animation_time model = case model.animation_state of
    MovingPieces _ start_time ->
        Ease.inOutCubic <| (tf ((posixToMillis model.time) - (posixToMillis start_time))) / animation_duration
    _ -> 0

update_animation model = case model.animation_state of
    MovingPieces mpieces start_time -> 
        let
            dt = animation_time model
            npieces = List.indexedMap (animate_piece start_time model) mpieces
        in
            if dt >= 1 then
                { model | animation_state = Interactive } |> find_all_arm_matches
            else
                { model | animation_state = MovingPieces npieces start_time }
    _ -> model

animate_piece start_time model i (piece,destination_piece) =
    let
        dt = animation_time model
        (x,y) = piece.position
        dist = V.len (V.sub destination_piece.position piece.position)
        delta = (V.sub destination_piece.position piece.position)
        nposition = V.add piece.position (V.smul dt delta)
    in
        ({ piece | position = nposition }, destination_piece)

next_level model =  case model.next_levels of
    level::rest -> (animate_pieces model.current_level.pieces level.pieces { model | previous_levels = model.current_level::model.previous_levels, current_level = level, next_levels = rest }, Cmd.none)
    [] -> new_graph { model | previous_levels = model.current_level::model.previous_levels, current_level = blank_level (model.current_level.n+1), next_levels = [], animation_state = ChangingLevel model.current_level }

-- versions that don't save the levels
--next_level = animate_level_change (\model -> new_graph { model | previous_levels = [], current_level = blank_level (model.current_level.n+1), next_levels = [] })
--previous_level = animate_level_change (\model -> new_graph { model | previous_levels = [], current_level = blank_level (model.current_level.n-1), next_levels = [] })

previous_level model =
    if model.current_level.n <= 3 then 
        (model,Cmd.none) 
    else
        case model.previous_levels of
            level::rest -> (animate_pieces model.current_level.pieces level.pieces { model | next_levels = model.current_level::model.next_levels, current_level = level, previous_levels = rest }, Cmd.none)
            [] -> new_graph { model | next_levels = model.current_level::model.next_levels, current_level = blank_level (model.current_level.n-1), previous_levels = [], animation_state = ChangingLevel model.current_level }

new_graph model = (model, make_level model)

set_mouse : Vector -> Model -> Model
set_mouse v model = { model | mouse = v }

move_touches : List TouchInfo -> Model -> Model
move_touches touches model = List.foldl (\touch -> \m -> case touch of
    TouchInfo id x y -> set_press_position ((==) (Touch id)) (x,y) m) model touches

update_press_sources : (PressSource -> Bool) -> (Press -> Press) -> List Press -> List Press
update_press_sources is_source = update_where (\p -> is_source p.source)

set_press_position : (PressSource -> Bool) -> Vector -> Model -> Model
set_press_position is_source pos model = 
    let
        update_press p =
            let
                d = V.len (V.sub pos p.start_position)
                nkind = case p.kind of
                    StaticPress -> if d > 0.1 then MovingPress else StaticPress
                    _ -> p.kind
            in
                { p | position = pos, kind = nkind }
    in
        { model | presses = update_press_sources is_source update_press model.presses }

after_move = move_pieces >> find_all_arm_matches

press_fold : (Press -> Model -> Model) -> Model -> Model
press_fold fn model = List.foldl fn model model.presses

move_pieces = press_fold (\press -> \model ->
    let
        level = model.current_level
        nlevel = 
            case press.action of
                DragPiece (i,op) -> 
                    let
                        speed = case model.selected_piece of
                            Nothing -> 1
                            Just _ -> 1/4
                        v = V.smul speed (V.sub press.position press.start_position)
                    in
                        { level | pieces = List.indexedMap (\j -> \p -> if j == i then { p | position = V.add v op.position } else p) level.pieces }
                _ -> level
    in
        { model | current_level = nlevel }
    )

smoosh_graph = Graph.smoosh 8 500

closest_edge (x,y) =
    if abs x > abs y then 
        if x < 0 then (-100, y) else (100, y)
    else
        if y < 0 then (x, -100) else (x, 100)

animate_pieces : (List Piece) -> (List Piece) -> Model -> Model
animate_pieces old_pieces new_pieces model =
    let
        n = max (List.length old_pieces) (List.length new_pieces)
        anim_pieces = List.map
            (\i -> case (listGet i old_pieces, listGet i new_pieces) of
                (Just p2, Just p) -> (p2,p)
                (Nothing, Just p) -> ({ p | position = closest_edge p.position }, p)
                (Just p2,_) -> (p2, { p2 | position = closest_edge p2.position })
                _ -> ({ position = (-100,-100), arms = [] }, { position = (-100,-100), arms = [] })
            )
            (List.range 0 (n-1))
    in
        { model | animation_state = MovingPieces anim_pieces model.time, selected_piece = Nothing }

set_graph g model =
    let
        pieces = (make_pieces >> arrange_pieces) g
        old_level = case model.animation_state of
            ChangingLevel o -> o
            _ -> model.current_level
        nmodel = { model | current_level = { graph = g, pieces = pieces, n = List.length pieces, arm_matches = [] }, selected_piece = Nothing } |> find_all_arm_matches
    in
        animate_pieces old_level.pieces nmodel.current_level.pieces nmodel

arrange_pieces pieces =
    let
        n = List.length pieces
        w = (sqrt >> ceiling) (tf n)
        place_piece i p =
            let
                x = modBy w i
                y = (i - x) // w
                size = view_width / (tf w)
                px = -view_width/2 + ((tf x)+0.5)*(size)
                py = -view_width/2 + ((tf y)+0.5)*(size)
            in
                { p | position = (px,py) }
    in
        List.indexedMap place_piece pieces

subs model = Sub.batch
    [ Browser.Events.onKeyDown decode_keypress
    , case model.animation_state of
        Interactive -> Time.every 100 Frame 
        _ -> Browser.Events.onAnimationFrame Frame
    ]

type alias Flags = D.Value

init : Flags -> Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init flags = \_ -> \_ -> 
--    let
--        q = Debug.log "flags" (D.decodeValue load_model flags)
--    in
        flags 
        |>
        (D.decodeValue (D.map (\m -> (m, Cmd.none)) load_model))
        >>
        (Result.withDefault (blank_model, make_level blank_model)) 

blank_level n =
    { graph = Graph.blank_graph
    , pieces = []
    , n = n
    , arm_matches = []
    }

blank_model =
    { current_level = blank_level 5
    , previous_levels = []
    , next_levels = []
    , animation_state = Interactive
    , presses = []
    , mouse = (0,0)
    , time = Time.millisToPosix 0
    , selected_piece = Nothing
    , show_hint = False
    }

tabbable = HA.attribute "tabindex" "0"

classList : List (String,Bool) -> Html.Attribute Msg
classList = SA.class << String.join " " << ((List.filter second) >> (List.map first))

line attr ((x1_,y1_),(x2_,y2_)) =
    Svg.line 
        (attr++
        [ SA.x1 <| ff x1_
        , SA.y1 <| ff y1_
        , SA.x2 <| ff x2_
        , SA.y2 <| ff y2_
        ])
        []

text size (x,y) content attr =
    Svg.text_
        ([ SA.x (ff x)
        , SA.y (ff y)
        , SA.textAnchor "middle"
        , SA.dominantBaseline "central"
        , SA.fontSize (ff size)
        , SA.style "user-select: none"
        ]++attr)
        [Svg.text content]

view : Model -> Document Msg
view model = 
    { title = "JIGGRAPH"
    , body = 
        [ div [ HA.class "app"
              , HE.on "keydown" keydown
              ]  
              ([ controls model
               , svg_bit model
               ]
              )
        ]
    }

controls model =
    div
        [ HA.id "controls"
        ]
        [ button 
            [ HE.onClick PreviousLevel
            , HA.disabled (model.current_level.n <= 3)
            , HA.id "next-level"
            ]
            [Html.text "-1"]
        , button 
            [ HE.onClick Reset
            , HA.id "reset"
            ]
            [Html.text "⤺"]
        , div
            [ HA.id "level-number"
            ]
            [ Html.text (fi model.current_level.n) ]
        , button 
            [ HE.onClick ToggleHint
            , HA.id "toggle-hint"
            ]
            [Html.text "?"]
        , button 
            [ HE.onClick NewRandomGraph
            , HA.id "re-randomise"
            ]
            [Html.text "🎲"]
        , button 
            [ HE.onClick NextLevel
            , HA.id "next-level"
            ]
            [Html.text "+1"]
        ]

view_width = 40

stop_contextmenu = D.succeed 
    { message = Noop
    , stopPropagation = True
    , preventDefault = True
    }

svg_bit model =
    svg
        [ SA.width "400"
        , SA.height "100%"
        , SA.viewBox (strf "% % % %" (List.map ff [-view_width/2, -view_width/2, view_width, view_width]))
        , SE.on "svgmove" <| decode_event_vector MouseMove
        , SE.on "mouseup" (decode_mouse MouseUp)
        , SE.on "mousedown" (decode_mouse MouseDown)
        , SE.custom "contextmenu" stop_contextmenu
        , SE.on "svgtouchstart" <| decode_touch TouchStart
        , SE.on "svgtouchmove" <| decode_touch TouchMove
        , SE.on "svgtouchend" <| decode_touch TouchEnd
        ]

        (   [ bg ]
         ++ (if model.show_hint then [view_graph model.current_level.graph] else [view_pieces model])
        )

bg =
    let
        clumps = List.map (clump_of_grass) (List.range 0 250)
    in
        Svg.g
            [ SA.id "background"
            ]
            clumps
        
clump_param = 1.48309 --+ (first model.mouse)/1000000

clump_of_grass : Int -> Svg Msg
clump_of_grass k =
    let
        an = scatter 0 (2*pi) clump_param k
        r = (sqrt <| (tf k) * 7) * (scatter 0.95 1 2 k)
        pos = (r*(cos an), r*(sin an))
        n = floor (scatter 3 5 20 k)
        w = scatter 0.1 0.3 55 k
        h = 0.1
        blade j =
            let
                i = 1000*k + j
                an1 = (cos an2)*0.2 + (scatter -1 1 50 i) * pi/10 - pi/2
                an2 = scatter -pi pi 30 i
                length = scatter 0.8 1.2 40 i
                base = V.add pos (w*(cos an2), h*(sin an2))
                (bx,by) = base
                end = V.add base (V.smul length (cos an1, sin an1))
                (ex,ey) = end
            in
                strf "M % % L % %" (List.map ff [bx, by, ex, ey])
        d = String.join " " (List.map blade (List.range 0 n))
        lum = scatter 35 40 1.6 k
    in
        Svg.path
            [ SA.class "grass" 
            , SA.d d
            , SA.stroke ("hsl(123.9, 57.7%, "++(ff lum)++"%)")
            ]
            []

view_pieces model =
    let
        pieces = case model.animation_state of
            MovingPieces mpieces _ -> mpieces
            _ -> List.map (\p -> (p, p)) model.current_level.pieces
    in
        g [SA.id "pieces"] (List.indexedMap (view_piece model) pieces)

get_edge : Graph -> Edge -> Maybe Segment
get_edge graph (i,j) =
    let
        ma = listGet i graph.points
        mb = listGet j graph.points
    in
        Maybe.map2 pair ma mb

get_edges graph = List.filterMap (get_edge graph) graph.edges

view_graph graph =
    let
        edges = get_edges graph
    in
        g
            [ SA.class "graph"
            , SA.style "opacity: 0.1"
            ]
            [ g [] <| List.map (view_edge graph) <| List.map2 pair edges graph.edges
            , g [] <| List.indexedMap (view_point graph) graph.points
            ]

view_edge : Graph -> (Segment, Edge) -> Html Msg
view_edge model (edge,indices) = 
    g 
        [] 
        [ line 
            [classList [("edge",True)], SA.stroke "blue"]
            edge
        ]

circle r (x,y) attr = Svg.circle
    (attr++[ SA.r <| ff r
           , SA.cx <| ff x
           , SA.cy <| ff y
           ]
    )
    []

view_point : Graph -> Int -> Point -> Svg Msg
view_point graph i point =
    g 
        []
        [ circle 1 point [ classList [("point",True)] ]
        ]

angle_to_piece from to =
    let
        (dx,dy) = V.sub to.position from.position
    in
        atan2 dy dx

distance_to_piece from to = (V.len (V.sub from.position to.position))

angle_fit_tolerance = 0.1

find_all_arm_matches model =
    let
        level = model.current_level
        arm_matches = List.map (\piece -> List.map (\angle -> find_arm_match piece angle level) piece.arms) level.pieces
        nlevel = { level | arm_matches = arm_matches }
    in
        { model | current_level = nlevel }

-- given a piece and an arm angle, find the other piece and its edge that fits.
-- Returns (piece_index, arm_index)
find_arm_match : Piece -> Float -> Level -> Maybe ArmMatch
find_arm_match piece angle level = 
    let
        possible_arms p2 = p2.arms |> List.indexedMap pair >> List.filter (\(j,a) -> angle_difference angle (a+pi) < angle_fit_tolerance) >> List.sortBy (\(j,a) -> angle_difference angle (a+pi))
        other_pieces = List.indexedMap pair >> List.filter (\(j,p2) -> p2 /= piece && not (List.isEmpty (possible_arms p2)) && angle_isclose angle_fit_tolerance angle (angle_to_piece piece p2)) >> List.sortBy (\(j,p2) -> distance_to_piece piece p2) <| level.pieces
    in
        Maybe.andThen (\(j,p2) -> Maybe.map (\(k,a) -> {piece = p2, piece_index = j, arm = a, arm_index = k}) (List.head (possible_arms p2))) (List.head other_pieces)

get_arm_match : Int -> Int -> Level -> Maybe ArmMatch
get_arm_match piece_index arm_index level =
    listGet piece_index level.arm_matches |> Maybe.andThen (listGet arm_index) >> Maybe.andThen identity

arm_fits_with : Int -> Int -> Level -> Maybe ArmMatch
arm_fits_with piece_index arm_index level = get_arm_match piece_index arm_index level |> Maybe.andThen 
    (\m1 -> 
        case get_arm_match m1.piece_index m1.arm_index level of
            Just m2 -> if m2.piece_index == piece_index && m2.arm_index == arm_index then Just m1 else Nothing
            _ -> Nothing
    )

arm_fits : Int -> Int -> Level -> Bool
arm_fits piece_index arm_index level = (arm_fits_with piece_index arm_index level) /= Nothing

piece_fits level piece_index piece = List.all (\(ei,angle) -> arm_fits piece_index ei level) (List.indexedMap pair piece.arms)

all_pieces_fit level = List.all (\(piece_index,piece) -> piece_fits level piece_index piece) (List.indexedMap pair level.pieces)

apply_skin_tone i s = 
    let
        toner = case i of
            0 -> "🏻"
            1 -> "🏼"
            2 -> "🏽"
            3 -> "🏿"
            _ -> ""
    in
        s ++ toner

skin_tone_color i =
    case i of
        0 -> "hsl(23.4, 71.9%, 88.8%)"
        1 -> "hsl(35.6, 77.1%, 79.4%)"
        2 -> "hsl(27.3, 47.8%, 68.4%)"
        3 -> "hsl(20.3, 33.3%, 36.5%)"
        _ -> "black"
skin_tone_color_darker i =
    case i of
        0 -> "hsl(23.4, 71.9%, 68.8%)"
        1 -> "hsl(35.6, 77.1%, 59.4%)"
        2 -> "hsl(27.3, 47.8%, 48.4%)"
        3 -> "hsl(20.3, 33.3%, 16.5%)"
        _ -> "black"

view_piece : Model -> Int -> (Piece, Piece) -> Svg Msg
view_piece model i (piece,destination_piece) =
    let
        is_selected = (Maybe.map first model.selected_piece) == Just i
        not_selected = model.selected_piece /= Nothing && not is_selected
        dt = animation_time model
        (x,y) = piece.position
        dot = circle 1 piece.position
            [ classList [("dot",True)]
            ]
        num_fitting_angles = List.length <| List.filter (\(ei,angle) -> arm_fits i ei model.current_level) (List.indexedMap pair piece.arms)
        skin_tone = apply_skin_tone (modBy 4 i)
        face_emoji = case model.animation_state of
            Interactive -> 
                if num_fitting_angles == 0 then
                    "😐"
                else if num_fitting_angles == List.length piece.arms then
                    if all_pieces_fit model.current_level then "🥳" else "😀"
                else
                    "🙂"
            _ -> "🤔"
        face = text 2 (x,y) face_emoji []
        view_arm ei =
            let
                (mangle1,mangle2) = (listGet ei piece.arms, listGet ei destination_piece.arms)
                (angle1,angle2) = case (mangle1,mangle2) of
                    (Just a1, Just a2) -> (a1,a2)
                    (Just a1, Nothing) -> (a1,a1)
                    (Nothing, Just a2) -> (a2,a2)
                    _ -> (0,0)
                size_for_mangle mangle = case mangle of
                    Just _ -> 1
                    Nothing -> 0
                size1 = size_for_mangle mangle1
                size2 = size_for_mangle mangle2

                angle = lerp angle1 angle2 dt
                closest_piece = Maybe.map (\m -> m.piece) (arm_fits_with i ei model.current_level)
                dangle = Maybe.withDefault angle (Maybe.map (angle_to_piece piece) closest_piece)
                unconnected_size = 3
                size = case model.animation_state of
                    Interactive -> Maybe.withDefault unconnected_size (Maybe.map (\p2 -> -0.05 + (distance_to_piece piece p2)/2) closest_piece)
                    _ -> (lerp size1 size2 dt) * unconnected_size
                fits = case model.animation_state of
                    Interactive -> arm_fits i ei model.current_level
                    _ -> False
                end = V.add piece.position (V.smul size (cos dangle, sin dangle))
                edge =
                    g
                        [ classList 
                            [("edge-part",True)
                            ,("fits", fits)
                            ]
                        ]
                        [ line 
                            [ SA.class "shadow"
                            , SA.stroke (skin_tone_color_darker (modBy 4 i))
                            ]
                            (piece.position, end)
                        , line 
                            [ SA.stroke (skin_tone_color (modBy 4 i))
                            ]
                            (piece.position, end)
                        ]
                dir = case (indexOf piece model.current_level.pieces, Maybe.andThen (\p2 -> indexOf p2 model.current_level.pieces) closest_piece) of
                    (Just a, Just b) -> a<b
                    _ -> True
                symbol = if fits then if dir then "✋" else "🤚" else "✊"
                handshake = 
                    text 
                        (if fits then 1.5 else 0.8) (0,0) (skin_tone symbol)
                        [ SA.transform (strf "translate(% %) rotate(%)" [ff (first end), ff (second end), ff (dangle*180/pi+90)]) ]
            in
                g
                    []
                    [edge, handshake]
        select_halo =
            circle 1.5 piece.position
                [ SA.class "selection-halo"
                , SA.fill "hsla(0,0%,100%,0.6)" 
                ]
        edge_indices = (List.range 0 ((max (List.length piece.arms) (List.length destination_piece.arms))-1))
        edges = g [SA.class "edges"] (List.map view_arm edge_indices)
    in
        g 
            [ classList 
                [ ("piece",True)
                , ("fits", piece_fits model.current_level i piece)
                , ("not-selected", not_selected)
                , ("selected", is_selected)
                ]
            ]
            ([edges]++(if is_selected then [select_halo] else [])++[face])

save : (Model, Cmd Msg) -> (Model, Cmd Msg)
save (model,cmd) = (model, Cmd.batch [cmd, (save_model >> saveLocalStorage) model])

save_model : Model -> E.Value
save_model model = E.object
    [ ("previous_levels", E.list save_level model.previous_levels)
    , ("next_levels", E.list save_level model.next_levels)
    , ("current_level", save_level model.current_level)
    ]

save_level : Level -> E.Value
save_level level = E.object
    [ ("n", E.int level.n)
    , ("graph", save_graph level.graph)
    , ("pieces", E.list save_piece level.pieces)
    ]

save_graph : Graph -> E.Value
save_graph g = E.object
    [ ("points", E.list save_point g.points)
    , ("edges", E.list (\(i,j) -> E.list E.int [i,j]) g.edges)
    ]

save_point (x,y) = E.list E.float [x,y]

save_piece : Piece -> E.Value
save_piece piece = E.object
    [ ("position", save_point piece.position)
    , ("arms", E.list E.float piece.arms)
    ]

load_model : D.Decoder Model
load_model = D.map find_all_arm_matches <|
    D.map3 (\previous_levels -> \next_levels -> \current_level -> { blank_model | previous_levels = previous_levels, next_levels = next_levels, current_level = current_level })
        (D.field "previous_levels" (D.list load_level))
        (D.field "next_levels" (D.list load_level))
        (D.field "current_level" load_level)

load_level : D.Decoder Level
load_level =
    D.map3 (\n -> \graph -> \pieces -> { n = n, graph = graph, pieces = pieces, arm_matches = [] })
        (D.field "n" D.int)
        (D.field "graph" load_graph)
        (D.field "pieces" (D.list load_piece))

load_graph: D.Decoder Graph
load_graph = 
    D.map2 (\points -> \edges -> { points = points, edges = edges })
        (D.field "points" (D.list load_point))
        (D.field "edges" (D.list load_edge))

load_pair: D.Decoder a -> D.Decoder (a,a)
load_pair d =
    let
        get_list list =
            case (List.head list, List.head (List.drop 1 list)) of
                (Just i, Just j) -> D.succeed (i,j)
                _ -> D.fail "Expected two things of the same type"
    in 
        D.andThen get_list (D.list d)

load_point = load_pair D.float
load_edge = load_pair D.int

load_piece =
    D.map2 Piece
        (D.field "position" load_point)
        (D.field "arms" (D.list D.float))
