
open System.Text.RegularExpressions
let (|FirstRegexGroup|_|) pattern input =
   let m = Regex.Match(input,pattern)
   if (m.Success) then Some m.Groups.[1].Value else None
   
// create a function to call the pattern
let testRegex str =
    match str with
    | FirstRegexGroup "http://(.*?)/(.*)" host ->
           printfn "The value is a url and the host is %s" host
    | FirstRegexGroup ".*?@(.*)" host ->
           printfn "The value is an email and the host is %s" host
    | _ -> printfn "The value '%s' is something else" str

// test
testRegex "http://google.com/test"
testRegex "alice@hotmail.com"


// setup the active patterns
let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None

// the main function
let fizzBuzz i =
  match i with
  | MultOf3 & MultOf5 -> printf "FizzBuzz, "
  | MultOf3 -> printf "Fizz, "
  | MultOf5 -> printf "Buzz, "
  | _ -> printf "%i, " i

// test
[1..20] |> List.iter fizzBuzz




// Location and store. 
let mutable x = 1
// Environment: x → loc1
// Store:       loc1 → 1

x <- 7  // Store opdateres: loc1 → 7

// Immutable gennemgang 
type t = { mutable a: int }
let r = { a = 5 }
r.a <- 10   // ændrer r.a i store

// arrays 
let a = Array.create 5 0   // [|0;0;0;0;0|] 
a.[2] <- 99                 // mutation på index 2
a.[2]                       // læs: 99
a.[4] <-10
a.[4]

// while loop hvor vi bruger imparative. 
let mutable i = 0 // du SKAL  erklære i som mutable,
while i < 5 do
    printfn "i = %d" i
    i <- i + 1 // 4 * (3 * (2 * (1 * 1)))   <- nu kan vi ENDELIG folde sammen

let rec factA = function
    | (0, m) -> m
    | (n, m) -> factA(n-1, n*m)  // <-- tail cal, ingen ventende operation, stack frame genbruges


// count with continuation. 

type Tree = Leaf 
    | Node of Tree * int * Tree

let rec countC t c =
    match t with
    | Leaf -> c 0
    | Node(tl, _, tr) -> 
        countC tl (fun vl ->
            countC tr (fun vr ->
                c(vl + vr + 1)))

let mittræ = Node(Node(Leaf, 1, Leaf), 2, Leaf)

let resultat = countC mittræ id
// resultat = 2

// normalt — ventende arbejde på stack
count tl + count tr + 1
// skal vente på count tl, så vente på count tr, så lægge sammen

// med continuation — ventende arbejde i en funktion på heap
countC tl (fun vl ->       // "når vl er klar, gør dette:"
    countC tr (fun vr ->   // "når vr er klar, gør dette:"
        c(vl + vr + 1)))   // "læg sammen og giv videre"

(*
Den går hele vejen ned i venstre side først, til den rammer Leaf. Derefter folder den tilbage.
fsharp// træet
Node(4)
├── Node(2)
│   ├── Node(1)
│   │   ├── Leaf  ← rammer bunden her først
```

Rækkefølgen er:
```
ned:    Node(4) → Node(2) → Node(1) → Leaf
tilbage: Leaf giver 0 → Node(1) får vl=0 → går ned i højre Leaf → osv.


En normal rekursiv funktion bruger et stack frame per kald.
Stacken har en fast grænse,
 typisk omkring 10.000-100.000 frames. Et meget dybt træ sprænger den:
// dette crasher på et træ med 200.000 noder
let rec count = function
    | Leaf -> 0
    | Node(tl, _, tr) -> count tl + count tr + 1

*)



// ASSIGNMENT 
let xs = [1;2]

let rec g = function
    | 0 -> xs
    | n ->
        let ys = n :: g(n-1)
        let result = List.rev ys
        printfn "n=%d  ys=%A  result=%A" n ys result
        result
// val g : int-> int list
g 2;;

// g(0) returnerer xs som er [1;2].
// når g(1) skriver n :: g(n-1) bliver det 1 :: g(0) = 1 :: [1;2] = [1;1;2].

let zs = let xs = [1;2]
let ys = [3;4]
xs@ys;;


// normal rec 
let rec f n =
match n with
    | 0->0
    | n-> f(n-1) + n;;
let x = f 3;;

// tail rec
let rec f n acc =
    match n with
    | 0 -> acc
    | n -> f (n-1) (acc + n)

let x = f 3 0

// PUSH: f(3, 0)
// PUSH: f(2, 3)  ← acc = 0+3
// PUSH: f(1, 5)  ← acc = 3+2
// PUSH: f(0, 6)  ← acc = 5+1

// stack: [ n=3, acc=0 ]
// stack: [ n=2, acc=3 ]  ← samme frame, n og acc overskrevet
// stack: [ n=1, acc=5 ]  ← samme frame, n og acc overskrevet
// stack: [ n=0, acc=6 ]  ← samme frame, n og acc overskrevet
// returnerer 6
// stack: []


let xs = [1;2];;
    let rec g = function
    | 0-> xs
    | n-> let ys = n::g(n-1)
            List.rev ys;;
// val g : int-> int list
g 2;;

sf3 { n=0, result=? }
sf2 { n=1, result=? }
sf1 { n=2, result=? }
sf0 { xs, g }


stack:
sf0 { xs, g }

heap:
xs  → [1;2]
ys  → [1;1;2]   ← unreachable, GC cleans up
rev → [2;1;1]   ← unreachable, GC cleans up
ys  → [2;2;1;1] ← unreachable, GC cleans up
rev → [1;1;2;2] ← final result

// Ja præcis. LIFO — Last In First Out. Det sidste frame der pushes er det første der poppes.
PUSH g(2)
PUSH g(1)
PUSH g(0)   ← øverst, popper først

POP g(0)    → returnerer [1;2]
POP g(1)    → laver ys, List.rev, returnerer
POP g(2)    → laver ys, List.rev, returnerer