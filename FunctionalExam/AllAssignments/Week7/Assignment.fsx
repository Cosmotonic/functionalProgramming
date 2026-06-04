
// Exercise 8.1 HR exercise 9.1
// Exercise 8.1 HR exercise 9.1
// Exercise 8.1 HR exercise 9.1
let xs = [1;2]

let rec g = function
    | 0 -> xs
    | n ->
        let ys = n :: g(n-1)
        List.rev ys

g 2

(*
Stack is built up through recursive calls
g(0) returns xs which is already on the heap
For each POP, new lists are allocated on the heap via :: and List.rev

Stack is built up through recursive calls first:
sf3 { n=0, result=? }
sf2 { n=1, result=? }
sf1 { n=2, result=? }
sf0 { xs, g }

Then the stack is emptied via POP, allocating results on the heap:

heap:
xs  → [1;2]
ys  → [1;1;2]   ← unreachable, GC cleans up
rev → [2;1;1]   ← unreachable, GC cleans up
ys  → [2;2;1;1] ← unreachable, GC cleans up
rev → [1;1;2;2] ← final result
*)


// Exercise 8.2 HR exercise 9.3
// Exercise 8.2 HR exercise 9.3
// Exercise 8.2 HR exercise 9.3
// sum(m,n)= m+(m+1)+(m+2)+···+(m+(n−1))+(m+n)
// sum(m, n) = sum(m, n-1) + (m + n)
let rec sum = function
    | (m, 0) -> m
    | (m, n) -> sum(m, n-1) + (m + n)

// sum(1, 2)
// = sum(1, 1) + (1+2)
// = sum(1, 0) + (1+1) + 3
// = 1         + 2     + 3
// = 6

let rec sumA = function 
    | (acc, m, 0) -> acc + m
    | (acc, m, n) -> sumA(acc + (m+n), m, n-1)

// iterative recursion is tail recursion. 

// Exercise 8.3 HR exercise 9.4.
// Exercise 8.3 HR exercise 9.4.
// Exercise 8.3 HR exercise 9.4.

// normal version 
let rec listL = function 
    | [] -> 0
    | _::xs -> 1 + listL(xs) // 1+1+1+1+0:: remaing list


let rec listA = function 
    | acc, [] -> acc 
    | acc, _::xs -> listA(acc+1, xs)
let objs = ["a"; "k"; "5"]
listA ( 0, objs)


// Exercise 8.4 HR exercise 9.6.
// Exercise 8.4 HR exercise 9.6.
// Exercise 8.4 HR exercise 9.6.
// Normal fact, uden akkumulator
let rec fact n =
    match n with
    | 0 -> 1
    | n -> n * fact(n-1) // multiply * happens before the recursive call = UDEN akkumulator

// tail recursive / iterative recursion
let rec factB = function
    | (0,m)-> m
    | (n,m)-> factB(n-1, n*m);;

factB (3,2)

// continuation - run a function inside, always end on a recursive call making it tail recursive. 
let rec factD n k =
    if n = 0 then k 1
    else factD (n-1) (fun result -> k (n * result))

factD 4 ( fun x -> x) 


// Exercise 8.5 - HR 8.6 
// & 
// Exercise 8.6 - HR 9.7

// Fibunacci 
// without whle loop 
let rec fibA n n1 n2 =
    match n with 
    | 0 -> n2                       // base case: vi er nået til F0, returner akkumulator
    | _ -> fibA (n-1) (n1+n2) n1    // ryk én plads: næste n1 = n1+n2, næste n2 = gamle n1

fibA 6 1 0 // hardcoded 1 0 
let fibW input = 
    let mutable x = 0
    let mutable m = 0
    let mutable n = 1
    while x <input do 
        let temp = m
        m<-n
        n<-temp+n
        x<-x+1
    m
fibW 6

let rec fibD = function 
    | (0, n1, n2) -> n1+n2
    | (n, n1, n2) -> fibD(n-1, n2, (n1+n2))

fibD (6, -1, 1)