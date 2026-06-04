// Adjust path below to reference the VectorSimple.dll file relative to your current directory
#r @"./bin/Debug/net8.0/VectorSimple.dll"

open VectorSimple

let a = make(1.0,-2.0)
printfn "a = %A" a

let b = make(3.0,4.0)
printfn "b = %A" b

let c = 2.0 *. a -. b
printfn "c = %A" c

printfn "coord c = %A" (coord c)

let d = c &. a
printfn "d = %A" d

let e = norm b
printfn "e = %A" e


