module Vector exposing (..)

type alias Vector = (Float, Float)

add (x1,y1) (x2,y2) = (x1+x2, y1+y2)
sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)
len (x,y) = sqrt (x*x + y*y)
smul s (x,y) = (s*x, s*y)

midpoint (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

normalise (x,y) =
    let
        d = len (x,y)
    in
        (x/d, y/d)

normal (x,y) = normalise (-y,x)

dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

sum : List Vector -> Vector
sum = List.foldl add (0,0)

point_line_distance p (p1,p2) =
    let
        v1 = sub p2 p1
        v2 = sub p p1
        n = normal v1
        d1 = len v1
        alpha = (dot v1 v2) / d1
        d = len (sub p (add p1 (smul alpha v1)))
    in
        if alpha<0 then len (sub p p1) else if alpha>d1 then len (sub p p2) else d
