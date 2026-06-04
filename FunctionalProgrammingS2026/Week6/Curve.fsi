module Curve 
[<Sealed>]
type Curve =
    static member ( + ) : Curve * Curve -> Curve
    static member ( * ) : float * Curve -> Curve
    static member ( |ˆ) : Curve * float -> Curve
    static member ( |ˆ) : Curve * int   -> Curve
    static member (-->) : Curve * (float * float) -> Curve
    static member (><)  : Curve * float -> Curve
val point       : float * float -> Curve
val verticRefl  : Curve -> float -> Curve
val boundingBox : Curve -> (float * float) * (float * float)
val width
val height
val toList
: Curve -> float
: Curve -> float
: Curve -> (float * float) list
