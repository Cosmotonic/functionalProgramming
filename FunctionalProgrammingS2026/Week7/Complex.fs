
type Complex = C of float * float
    with 
    static member (+) (C(a,b), C(c,d)) = C(a+c, b+d)
    static member (-) (C(a,b), C(c,d)) = C(a-c, b-d)
    static member (*) (C(a,b), C(c,d)) = C(a*c - b*d, a*d + b*c)
    static member (/) (C(a,b), C(c,d)) = C((a*c + b*d) / (c*c + d*d), (b*c - a*d) / (c*c + d*d))


let make (value1 : float) (value2 : float) : Complex =
    C(value1, value2)

let real complex =
    match complex with
    | C(r, _) -> r

let imaginary complex =
    match complex with
    | C(_, r) -> r
