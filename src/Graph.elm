module Graph exposing (..)
import Tuple exposing (pair, first, second)
import Dict
import Random
import Random.Array
import Random.List
import Array
import Util exposing (..)
import Vector as V

-- Utility functions

blank_graph = { points = [], edges = [] }

remove_index n list = (List.take n list)++(List.drop (n+1) list)

type alias Point = (Float,Float)

type alias Edge = (Int,Int)

type alias Segment = (Point,Point)

type alias Graph = 
    { points : List Point
    , edges : List Edge
    }

-- Construct a Graph
graph points edges = { points = points, edges = edges }

get_edge : Graph -> Edge -> Maybe Segment
get_edge g (i,j) =
    let
        ma = listGet i g.points
        mb = listGet j g.points
    in
        Maybe.map2 pair ma mb

get_edges g = List.filterMap (get_edge g) g.edges

find_intersection : Bool -> (Segment, Segment) -> Maybe (Segment,Segment,Point)
find_intersection must_be_on_segment (e1, e2) =
    let
        ((x1,y1),(x2,y2)) = e1
        ((x3,y3),(x4,y4)) = e2
        dx1 = x2 - x1
        dy1 = y2 - y1
        dx2 = x4 - x3
        dy2 = y4 - y3
        dx3 = x3 - x1
        dy3 = y3 - y1
        a = (dx3*dy2 - dy3*dx2)/(dx1*dy2 - dy1*dx2)
        b = -(dx3*dy1 - dy3*dx1)/(dx2*dy1 - dx1*dy2)
        x = x3 + b*dx2
        y = y3 + b*dy2
        wiggle = 0.0001
        min = wiggle
        max = 1-wiggle
    in
        if not must_be_on_segment || (b>min && b<max && a>0 && a<max) then Just (e1,e2,(x,y)) else Nothing

segments_intersect a b = case (find_intersection True (a,b)) of
    Nothing -> False
    Just _ -> True

random_point r = Random.map2 pair (Random.float -r r) (Random.float -r r)
random_segment = Random.map2 pair (random_point 1) (random_point 1)

-- Remove edges from a graph until it is planar
planar_graph : Graph -> Graph
planar_graph g = 
    let
        intersects a others = 
            let
                segments = List.filterMap get_segment others
                ms = get_segment a
            in
                Maybe.withDefault False (Maybe.map (\s -> List.any (segments_intersect s) segments) ms)
        get_point i = listGet i g.points
        get_segment (a,b) = Maybe.map2 pair (get_point a) (get_point b)
        kept_edges = List.foldl (\a -> \keep -> if intersects a keep then keep else a::keep) [] g.edges
    in
        { points = g.points, edges = kept_edges}

-- Map segments (point,point) to the indices of those points in the given list
segments_to_edges points segments =
    let
        ppairs = List.indexedMap pair points
        comp a b = (a==b)
        find_point p = List.filterMap (\(i,p2) -> if (comp p p2) then Just i else Nothing) (ppairs) |> List.head
        segment_to_edge (p1,p2) = Maybe.map2 pair (find_point p1) (find_point p2)
        edges = List.filterMap segment_to_edge segments
    in
        edges


-- n random points in the square (-r,-r)--(r,r)
random_points r n = Random.map Array.toList (Random.Array.array n (random_point r))

-- A random planar graph with n vertices
random_planar_graph : Int -> Random.Generator Graph
random_planar_graph n = 
    let
        points = Random.andThen Random.List.shuffle (random_points 20 n)
        all_edges = pairs (List.range 0 n) |> Random.List.shuffle
        pg = Random.map2 (\p -> \e -> planar_graph {points = p, edges = e}) points all_edges
    in
        Random.map (ensure_max_degree 5) pg

degree_of : Graph -> Int -> Int
degree_of g n = (List.length << List.filter (\(i,j) -> i==n || j==n)) g.edges

-- Ensure that every vertex in the graph has no more than the given degree, by removing edges.
-- Edges between vertices with the greatest degree are removed first.
ensure_max_degree : Int -> Graph -> Graph
ensure_max_degree maximum g =
    let
        restrict n g2 = 
            let
                dn = degree_of g2 n
                (n_edges, other_edges) = List.partition (\(i,j) -> i==n || j==n) g2.edges
                biggest_edges = List.sortBy (\(i,j) -> degree_of g2 j) (List.map (\(i,j) -> if i==n then (i,j) else (j,i)) n_edges)
                keep_edges = (List.take maximum biggest_edges)++other_edges
            in
                {g2 | edges = keep_edges }
        ng = List.foldl restrict g (List.range 0 (List.length g.points))
    in
        ng

ensure_min_angle_gap : Float -> Graph -> Graph
ensure_min_angle_gap gap g =
    let
        edge_angle (i,j) = case (listGet i g.points, listGet j g.points) of
            (Just p1, Just p2) ->
                let
                    (dx,dy) = V.sub p2 p1
                in
                    atan2 dy dx
            _ -> 0

        angles_close a b = angle_isclose gap a b || angle_isclose gap a (b+pi)

        restrict i g2 =
            let
                edges =
                    List.filter
                        (\(j,k) -> i==j || i==k) 
                        g.edges
                can_remove e e2 =
                    let
                        (j,k) = e2
                        d = min (degree_of g2 j) (degree_of g2 k)
                    in
                        e /= e2 && d>2 && (angles_close (edge_angle e) (edge_angle e2))
                close_edges e = List.filter (can_remove e) edges
                remove_edges = List.foldl (\e -> \remove -> (close_edges e)++remove) [] edges
            in
                { g2 | edges = List.filter (\e -> not (List.member e remove_edges)) g2.edges }
    in
        List.foldl restrict g (List.range 0 (List.length g.points))

smoosh : Float -> Int -> Graph -> Graph
smoosh minDist steps g =
    let
        strength = 0.5
        smoosh_step _ g2 =
            { g2 | points = List.map (smoosh_point g2) g2.points }
        smoosh_point g2 p =
            let
                repel_point p2 =
                    let
                        mid = V.midpoint p p2
                        diff = V.sub p mid
                        d = V.len diff
                        md = max 0 (minDist/2-d)
                        (tx,ty) = V.smul md (V.normalise diff)
                    in
                        if p2 == p then (0,0) else V.smul strength (tx,ty)
                move_away_from_points = List.foldl (\p2 -> \diff -> V.add diff (repel_point p2)) (0,0) g2.points

                repel_edge e =
                    case get_edge g2 e of
                        Just (p1,p2) ->
                            let
                                v1 = V.sub p2 p1
                                v2 = V.sub p p1
                                n = V.normal v1
                                d1 = V.len v1
                                alpha = (V.dot v1 v2) / d1
                                p3 = V.add p1 (V.smul alpha (V.normalise v1))
                                d = V.len (V.sub p p3)
                                td = max 0 (minDist - d)
                                ts = sqrt (td/minDist)
                            in
                                if alpha<0 || alpha>d1 then
                                    (0,0)
                                else 
                                    if p1==p || p2==p then (0,0) else V.smul (td*0.1*ts) (V.normalise (V.sub p p3))
                        _ -> (0,0)

                move_away_from_edges = List.foldl (\e -> \diff -> V.add diff (repel_edge e)) (0,0) g2.edges

                move_to_centre = V.smul -0.01 p
            in
                V.sum 
                    [ p
                    , move_away_from_points
                    , move_away_from_edges
                    , move_to_centre
                    ]
    in
        List.foldl smoosh_step g (List.repeat steps 0)
