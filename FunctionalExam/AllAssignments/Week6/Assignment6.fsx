
// from presentaiton slides. 
type aExp =
    | N of int
    | V of string
    | Add of aExp * aExp (* addition *)
    | Mul of aExp * aExp (* multiplication *)
    | Sub of aExp * aExp (* subtraction *)
(* Arithmetical expressions *)
(* numbers *)
(* variables *)

type bExp =
    | TT 
    | FF 
    | Eq of aExp * aExp
    | Lt of aExp * aExp (* less than *)
    | Neg of bExp
    | Con of bExp * bExp

type stm =
    | Ass of string * aExp
    | Skip
    | Seq of stm * stm
    | ITE of bExp * stm * stm
    | While of bExp * stm
    | RU of bExp * stm
    | IT of bExp * stm

let rec A a s =
    match a with
    | N n        -> n 
    | V x        -> Map.find x s
    | Add(a1,a2) -> A a1 s + A a2 s
    | Mul(a1,a2) -> A a1 s * A a2 s
    | Sub(a1,a2) -> A a1 s - A a2 s

let expr = Add(N 2, N 3)
A expr Map.empty

let rec B b s = // boolean
    match b with 
    | TT -> true
    | FF -> false 
    | Eq (a1, a2) -> A a1 s = A a2 s    
    | Lt (a1, a2) -> A a1 s < A a2 s // lt = less than 
    | Neg b1 -> not(B b1 s)
    | Con (b1, b2) -> B b1 s && B b2 s

let update x v s = Map.add x v s

let rec I stm s =
    match stm with
    | Ass(x,a)         -> update x (A a s) s // assignment
    | Skip             -> s
    | Seq(stm1, stm2)  -> I stm2 (I stm1 s)
    | ITE(b,stm1,stm2) -> if B b s then I stm1 s else I stm2 s 
    | While(b, stm)    -> if B b s then I (While(b, stm)) (I stm s) else s
    | RU(b, stm) -> let s2 = I stm s
                    if B b s2 then s2 else I (RU(b, stm)) s2 // Run Until s2 is true then return s2
    | IT (b, stm)-> if B b s then I stm s else I Skip s  // IT = IF THEN


// 5 Defined examples.  
let stmWhileUp = While(Neg(Eq(V "x", N 5)), Ass("x", Add(V "x", N 1))) // Run until the mapped value (3) reaches 0
let stateWhileUp =  Map.ofList [("x", 3)]
I stmWhileUp stateWhileUp

let stmSkip = Skip
let stateSkip = Map.ofList[("Counte", 1)]
I stmSkip stateSkip

let stmITE =ITE(Lt(N 4, V "counter"), Ass("result", N 1), Ass("result", N 0))
let stateITE = Map.ofList[("counter", 5)]
I stmITE stateITE

let stmWhile = While(Neg(Eq(V "x", N 0)), Ass("x", Sub(V "x", N 1))) // Run until the mapped value (3) reaches 0
let stateWhile =  Map.ofList [("x", 3)]
I stmWhile stateWhile

let stmSeq = Seq(Ass("result1", Add(N 50, N 50)), Ass("result2", Add(N 100, N 100)))
let stateSeq = Map.empty
I stmSeq stateSeq

// Testing extra stm patterns. 
let stmRU = RU(Eq(V "x", N 3), Ass("x", Add(V "x", N 1)))
let stateRU = Map.ofList [("x", 0)]
I stmRU stateRU

// Example from assignment
let stmt0 = Ass("result", Add(N 10, N 30))
let state0 = Map.empty
I stmt0 state0


// 6.3 
(*
"The problem is that F# is per definition immutable, if we generate an incrementinside 
the A function it will count as a new state(value). That new state would have to
be passed along where ever inc(x) inside of A would be used. As A is being called 
by both B and I this change would have a ripple effect."
*)