module Util exposing (..)
import Tuple exposing (first, second, pair)

listGet n = (List.drop n) >> List.head
pairs list = case list of
    [] -> []
    a::rest -> (List.map (\b -> (a,b)) rest)++(pairs rest)

tf = toFloat
ff = String.fromFloat
fi = String.fromInt

strf : String -> List String -> String
strf template bits =
    let
        next_bit cbits = case cbits of
            a::rest -> (a,rest)
            [] -> ("",[])
    in
        first <| List.foldl (\chr -> \(out,cbits) -> 
            if chr=='%' then
                let
                    (suffix,nbits) = next_bit cbits
                in
                    (out++suffix, nbits)
            else
                (out++(String.fromChar chr), cbits)
        ) ("",bits) (String.toList template)


update_where : (a -> Bool) -> (a -> a) -> List a -> List a
update_where test fn = List.map (\x -> if test x then fn x else x)

fmod : Float -> Float -> Float
fmod a b =
    let
        n = (toFloat << truncate) (a/b)
        r = a - n*b
    in
        r

indexOf : a -> List a -> Maybe Int
indexOf x = List.indexedMap pair >> List.filter (second >> ((==) x)) >> List.map first >> List.head

lerp : Float -> Float -> Float -> Float
lerp a b t = a + (b-a)*t

angle_difference a b =
    let
        d = fmod (a - b) (2*pi)
    in
        abs <| if d < -pi then d + 2*pi else if d > pi then d - 2*pi else d

angle_isclose tol rangle rtarget =
    let
        angle = fmod rangle (2*pi)
        target = fmod rtarget (2*pi)
        d = abs (target - angle)
        rd = if d > pi then abs (d - 2*pi) else d
    in
        rd <= tol


phi = ((sqrt 5) + 1)/2

scatter a b k i = a + (fmod (phi*((tf i)+1)*k) (b-a))

