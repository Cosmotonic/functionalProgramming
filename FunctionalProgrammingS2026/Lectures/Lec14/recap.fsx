( << );;
let ( << ) f g x = f(g x)

let inc x = x+1
inc <| 2
inc 2
2 |> inc

( |> )

let ( |> ) x f = f x
let ( <| ) f x = f x

// Slide 9
let xs = [1;2;3]
let [x;y;z] = xs
let y::ys = xs

let isMember x xs = List.exists (fun y -> y=x) xs
let insert x ys = if isMember x ys then ys else x::ys
let union xs ys =
  List.foldBack (fun x rs -> insert x rs) xs ys
let union xs ys = List.foldBack insert xs ys
let union xs = List.foldBack insert xs
let union = List.foldBack insert (* Error - Value restriction *)

// Slide on Mutable record field
type 'a ref = {mutable contents: 'a}
let ref v = {contents = v}
let (!) r = r.contents
let (:=) r v = r.contents <- v

let a = ref 5
let b = a
b := 10
!a
!b

// Slides on mutable versus ref
let mutable a' = 5
let mutable b' = a'
b' <- 10
a'
b'

// Efficiency

let rec fact = function
    0 -> 1
  | n -> n*fact(n-1)

let fact n =
  let rec factA = function
    (0,m) -> m
  | (n,m) -> factA (n-1, n*m)
  factA (n,1)

let rec naiveRev = function
    [] -> []
  | x::xs -> naiveRev xs @ [x]

let rev xs =
  let rec revA = function
      ([], ys) -> ys
    | (x::xs, ys) -> revA(xs, x::ys)
  revA (xs,[])

rev [1;2]

let rec fib = function
    0 -> 0
  | 1 -> 1
  | n -> fib(n-1) + fib(n-2)

let rec fibC n c =
  match n with
    0 -> c 0
  | 1 -> c 1
  | n -> fibC (n-1) (fun res1 -> fibC (n-2) (fun res2 -> c(res1 + res2)))
    


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


// Section 11.4 - Sieve of Eratosthenes

let sift a sq = Seq.filter (fun n -> n % a <> 0) sq

let rec sieve sq =
  Seq.delay (fun () -> 
               let p = Seq.item 0 sq
               Seq.append
                (Seq.singleton p)
                (sieve (sift p (Seq.skip 1 sq))))

let primes = sieve (Seq.initInfinite (fun n -> n+2))
let nthPrime n = Seq.item n primes
nthPrime 300

let primesCached = Seq.cache primes
let nthPrime' n = Seq.item n primesCached

#time
nthPrime' 300

nthPrime' 600

let rec sieve sq =
  seq { let p = Seq.item 0 sq
        yield p
        yield! sieve (sift p (Seq.skip 1 sq)) }

let primes = sieve (Seq.initInfinite (fun n -> n+2))
let nthPrime n = Seq.item n primes
nthPrime 500

let primesCached = Seq.cache primes
let nthPrime' n = Seq.item n primesCached
nthPrime' 700

// Source of examples:
//   https://docs.microsoft.com/en-us/dotnet/articles/fsharp/language-reference/active-patterns

let (|Even|Odd|) input =
  if input % 2 = 0 then Even else Odd

let TestNumber input =
   match input with
   | Even -> printfn "%d is even" input
   | Odd -> printfn "%d is odd" input

TestNumber 7
TestNumber 11
TestNumber 32

let (|Int|_|) (str:string) =
  match System.Int32.TryParse(str) with
  | (true,i) -> Some i
  | _ -> None

let (|Bool|_|) (str:string) =
  match System.Boolean.TryParse(str) with
  | (true,b) -> Some b
  | _ -> None

let testParse str = 
  match str with
  | Int i -> printfn "The value is an int '%i'" i
  | Bool b -> printfn "The value is a bool '%b'" b
  | _ -> printfn "The value '%s' is something else" str

testParse "12"
testParse "true"
testParse "abc"

let h x y = x+y
2 |> h <| 3

(2 |> h) <| 3

