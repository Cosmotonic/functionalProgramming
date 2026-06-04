// Adjust path below relative to your current directory
#r @"./bin/Debug/net8.0/VectorObj.dll"

open ObjVector
let a = ObjVector(1.0, -2.0)
printfn "a = %A" a

let b = ObjVector(Y=4.0, X=3.0)
printfn "b = %A" b

b.coord()

let c = 2.0 * a - b
printfn "c = %A" c

c.coord()

b.x

let d = c * a
printfn "d = %A" d

let e = b.norm()

let g = (+) a b

g.coord()
