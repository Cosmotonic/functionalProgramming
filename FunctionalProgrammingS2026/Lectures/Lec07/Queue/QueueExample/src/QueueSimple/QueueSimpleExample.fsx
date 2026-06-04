// Adjust below path relative to current path.

#r @"./bin/Debug/net8.0/QueueSimple.dll" 

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

let q4 = Queue.put 4 q3
printfn "q4 = %A" q4

let (x2,q5) = Queue.get q4
printfn "(x2,q5) = %A" (x2,q5)

let (x3, q6) = Queue.get q5
printfn "(x3,q6) = %A" (x3,q6)

(* Equality does not work *)
let qnew = Queue.put 2 q0
printfn "qnew = %A" qnew

let res = qnew = q3
printfn "qnew = q3 = %A" res
