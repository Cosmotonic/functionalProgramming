#r @"./bin/Debug/net8.0/Queue.dll" 

let q0' = Queue.empty
printfn "q0' = %A" q0'

let q0 = Queue.empty : Queue.Queue<int>
printfn "q0 = %A" q0

let q1 = Queue.put 1 q0
printfn "q1 = %A" q1

let q2 = Queue.put 2 q1
printfn "q2 = %A" q2

let (x,q3) = Queue.get q2
printfn "(x,q3) = %A" (x,q3)

(* Test of Equality *)
let qnew = Queue.put 2 q0
printfn "qnew = %A" qnew

let r = qnew = q3
printfn "qnew = q3 = %A" r

(* Test of Ordering *)
printfn "qnew < q3 = %A (expected false)" (qnew < q3)
printfn "qnew > q3 = %A (expected false)" (qnew > q3)
printfn "q1 < q2 = %A (expected true)" (q1 < q2)
