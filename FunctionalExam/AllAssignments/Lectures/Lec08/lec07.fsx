// Slide 7
let mutable x = 1
let mutable y = 3

let x = ref 1
let y = ref 3

// Slide 9
type intRec  = { mutable count : int }

let r1 = {count = 0}

let incr (x: intRec) =
    x.count <- x.count + 1
    x.count

incr r1

// Slide 10-11 on Mutable record field
type 'a ref = {mutable contents: 'a}
let ref v = {contents = v}
let (!) r = r.contents
let (:=) r v = r.contents <- v

let a = ref 5
let b = a
a := 10
!a
!b

let mutable a' = 5
let mutable b' = a'
b' <- 10
a'
b'


// Slide 12
// Restrictions on polymorphic expressions, Section 8.5:
let mutable a = [] // Not allowed if done a singular expression.
a <- 2::a // Ok, if done together with line above - the type becomes monomorphic.

let a = {contents = []} // Not allowed
let a = ref [] // Not allowed
let a() = {contents = []} // Allowed
1 :: a().contents // Allowed
'a' :: a().contents // Allowed
let a() = ref [] // Allowed
1 :: !a() // Allowed
'a' :: !a() // Allowed


// Slide 14
Unchecked.defaultof<int> // Example default value

// Sequential composition
let mutable x = 0
let y = (x;23)
let y = (ignore x;23)
let y = (x |> ignore; 23)

//let ignore x = ()

// Slide 15
// Arrays
let xs = [1;2]
let a = [|1;2|]

let a = Array.create 5 "a"
a.[2] <- "b"
a
a.[0]
a[0]  // After F# version 6 you can leave out the dot when indexing.

// Slide 18 - While loops
let mutable x = 0
while x < 10 do x <- x+1
let rec wh() = if x < 10 then (x<-x+1;wh()) else ()

// Slide 21 - Efficiency

let rec fact = function
    0 -> 1
  | n -> n*fact(n-1)

let rec fact n =
  match n with
    0 -> 1
  | n -> n*fact(n-1)

// Slide 23
let rec (@) xs ys =
  match xs with
    | [] -> ys
    | x::xs' -> x::(xs'@ys)

// Slide 24
let rec naiveRev = function
    [] -> []
  | x::xs -> naiveRev xs @ [x]

// Slide 26
let fact n =
  let rec factA (n,m) =
    match (n,m) with
      (0,m) -> m
    | (n,m) -> factA (n-1, n*m)
  factA (n,1)

let rec factA = function
    (0,m) -> m
  | (n,m) -> factA (n-1, n*m)


// Slide 27
let rec revA = function
    ([], ys) -> ys
  | (x::xs, ys) -> revA(xs, x::ys)

// Slide 30
let xs16 = List.init 100000 (fun i -> 2500)

#time

for i in xs16 do let _ = fact i in ()

for i in xs16 do let _ = factA (i,1) in ()

for i in xs16 do let _ = () in ()

// Slide 31
let xs20000 = [1 .. 20000]

naiveRev xs20000

revA (xs20000, [])
#time

// Slide 32
let xs = [5;6;7]
let ys = 3::4::xs
let zs = xs @ ys
let n = 27


// Slide 38
let rec bigList n = if n=0 then [] else 1::bigList (n-1)

bigList 120000

bigList 1200000  // Stack overflow, depending on your stack size.

let rec bigListA n xs = if n=0 then xs else bigListA (n-1) (1::xs)

let xsVeryBig = bigListA 12000000 []

(* Do not wait for completion on machine configured with unlimited process memory. *)
let xsTooBig = bigListA System.Int32.MaxValue []

(* Using general iterative function *)

// Slide 40
let rec factA (n,m) = if n <> 0 then factA(n-1, n*m) else m

(* Slide 40 - Example factA and g *)
let f (n,m) = (n-1,n*m)
let p (n,m) = n <> 0
let h (n,m) = m
let rec g z = if p z then g(f z) else h z

g (10,1)
factA (10,1)

(* Slide 40 - Example revA and g *)
let f (xs,ys) = (List.tail xs, (List.head xs)::ys)
let p (xs,ys) = not (List.isEmpty xs)
let h (xs,ys) = ys
let rec g z = if p z then g(f z) else h z

g ([1;2;3],[])
revA ([1;2;3],[])

// Slide 39
let factW n =
  let ni = ref n
  let r = ref 1
  while !ni>0 do
    r := !r * !ni; ni := !ni-1
  !r
factW 15

(* Try make factW' using mutable stack allocated variables instead of references
   and time again. *)
let factW n =
  let mutable ni = n
  let mutable r = 1
  while ni>0 do
    r <- r * ni; ni <- ni-1
  r
factW 15

// Slide 40
let rec revA (xs, ys) =
  if not (List.isEmpty xs)
  then revA(List.tail xs, (List.head xs)::ys)
  else ys

#time
for i in 1 .. 1000000 do let _ = factA (400,1) in ()

for i in 1 .. 1000000 do let _ = factW 400 in ()

// Slide 44
let rec fib = function
    0 -> 0
  | 1 -> 1
  | n -> fib(n-1) + fib(n-2)

let rec fibC n c =
  match n with
    0 -> c 0
  | 1 -> c 1
  | n -> fibC (n-1) (fun res1 -> fibC (n-2) (fun res2 -> c(res1 + res2)))
    

fib 40

let rec itfib(n,a,b) =
  if n <> 0
  then itfib(n-1,a+b,a)
  else a

itfib(40,0,1)

let rec bigListC n c =
  if n=0 then c []
  else bigListC (n-1) (fun res -> c(1::res))

(* id is a built in function *)

#time
bigListC 270000 id
bigListC 26000000 id
bigList 260000
bigList 300000
bigList 2400000 // Too big.


type 'a BinTree =
    Leaf
  | Node of 'a BinTree * 'a * 'a BinTree

let rec count = function
    Leaf -> 0
  | Node(tl,n,tr) -> count tl + count tr + 1

let rec countC t c =
 match t with
     Leaf -> c 0
   | Node(tl,n,tr) ->
       countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))

let ex = Node(Node(Leaf,1,Leaf),2,Node(Leaf,3,Leaf))
countC ex id
(* 
(* L1 *)    countC (Node(Node(Leaf,1,Leaf),2,Node(Leaf,3,Leaf))) (fun1 x->x)
(* L2 *) ~> countC (Node(Leaf,1,Leaf)) (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1)))

(* L3 *) ~> countC Leaf (fun3 vl3 -> countC Leaf (fun3 vr3 -> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) (vl3+vr3+1)))
         ~> (fun3 vl3 -> countC Leaf (fun3 vr3 -> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) (vl3+vr3+1))) 0
         ~> countC Leaf (fun3 vr3 -> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) (0+vr3+1))
         ~> (fun3 vr3 -> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) (0+vr3+1)) 0
         ~> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) (0+0+1)
         ~> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) 1

         ~> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (1+vr2+1))

(* L4 *) ~> countC Leaf (fun4 vl4 -> countC Leaf (fun vr4 -> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) (vl4+vr4+1)))
         ~> (fun4 vl4 -> countC Leaf (fun vr4 -> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) (vl4+vr4+1))) 0
         ~> countC Leaf (fun vr4 -> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) (0+vr4+1))
         ~> (fun vr4 -> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) (0+vr4+1)) 0
         ~> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) (0+0+1)
         ~> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) 1
         ~> (fun1 x->x) (1+1+1)
         ~> (fun1 x->x) 3
         ~> 3                      
*)
