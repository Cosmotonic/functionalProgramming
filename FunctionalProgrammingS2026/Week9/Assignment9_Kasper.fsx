// hand-in 9

// exercise 9.8
// exercise 9.8
// Develop a version of the counting function for binary trees
//         countA: int-> BinTree<’a>-> int
// that makes use of an accumulating parameter. Observe that this function is not tail recursive.
type binaryTree<'a> = 
    | Leaf
    | Node of binaryTree<'a> * 'a * binaryTree<'a>

//count function
let t5 = Node(Node(Leaf, 7, Leaf), 5, Node(Leaf, 7, Leaf));;

let rec count = function
    Leaf -> 0
  | Node(tl, _, tr) -> count tl + count tr + 1

//countA function
let rec countA (binaryTree) (acc) : int = 
    match binaryTree with
    | Leaf -> acc
    | Node (left, _, right) -> 
        countA right (countA left (acc + 1)) // continuation 

countA t5 0

// exercise 9.9
// exercise 9.9
// Declare a tail-recursive functions with the type
// countAC : BinTree<’a>-> int-> (int-> ’b)-> ’b
// such that count t = countAC t 0id.The intuition with countAC tacis that a is the
// number of nodes being counted so far and c is the continuation.
let rec countAC (t: BinTree<'a>) (acc: int) (cont: int -> 'b) :'b =
    match t with
    | Leaf -> cont acc
    | Node(tl, x, tr) ->
        countAC tl (acc + 1) (fun accAfterLeft ->
            countAC tr accAfterLeft cont)

countAC t5 0 (fun x -> printfn "Total nodes: %d" x; x)


//exercise 9.3 (9.10 in book)
//exercise 9.3 (9.10 in book)
    // so the problem is not in the call bigListK 130000 id,  we continue bigListK (n-1) until we reaches 0.
    // when we go to 0 we call k[], that is 130000 nested functions we call and that is what causes the stack overflow.
    // it calls func 1 -> func 2 -> func 3 -> .... func 130000. each of those calls is waiting for the next to return and that results in the stack overflow.        



// exercise 9.4 (9.11 in book)
// exercise 9.4 (9.11 in book)
// Declare tail-recursive functions leftTree and rightTree.ByuseofleftTree it should
// be possible to generate a big unbalanced tree to the left containing n +1values in the nodes so
// that n is the value in the root, n − 1 is the value in the root of the left subtree, and so on. All
// subtree to the right are leaves. Similarly, using rightTree it should be possible to generate a
// big unbalanced tree to the right.
// 1. Use these functions to show the stack limit when using count and countA from Exer
// cise 9.8.
// 2. Use these functions to test the performance of countC and countAC from Exercise 9.9.
//building leftTree that builds tree leaning to the left
let rec leftTree n =
    if n < 0 then Leaf
    else Node(leftTree(n-1), n, Leaf)
  
leftTree 5

let rec leftTreeAc n k =
    if n < 0 then k Leaf
    else leftTreeAc (n-1) (fun res -> k (Node(res, n, Leaf)))

leftTreeAc 5 (fun x -> x)

let rec rightTreeAc n k =
    if n < 0 then k Leaf
    else rightTreeAc (n-1) (fun res -> k (Node(Leaf, n, res)))

rightTreeAc 5 (fun x -> x)

// 1) testing count and CountA
let t6 = leftTreeAc 10000000 (fun x -> x)

count t6 
countA t6 0

countAC t6 0 (fun x -> x)


//exercise 9.4 (11.1 in book)
//exercise 9.4 (11.1 in book)
let idWithPrint i = let _ = printfn "%d" i 
                    i;; 
idWithPrint 3; 
let nat = Seq.initInfinite(fun i -> i);; // 
let nat = Seq.initInfinite idWithPrint;;

Seq.item 4 nat;; 
let even = Seq.filter(fun n -> n%2=0) nat;; 
let odds = Seq.initInfinite (fun i -> 2 * i + 1)
// i=0 → 1, i=1 → 3, i=2 → 5, etc.

//to see the first 10
odds |> Seq.take 10 |> Seq.toList
// [1; 3; 5; 7; 9; 11; 13; 15; 17; 19]


// keywords: active patterns 
// keywords: active patterns 
let (|Even|Odd|) input = 
    if input % 2 = 0 then Even else Odd

let TestNumber input = 
    match input with
    | Even -> printfn "%d is even" input
    | Odd -> printfn "%d is odd" input

TestNumber 7



//exercise 9.5 (11.2 in book)
//exercise 9.5 (11.2 in book)
let factorials = Seq.initInfinite (fun i -> 
    let rec fact n = if n = 0 then 1 else n * fact(n-1)
    fact i)

//to see the first 8
factorials |> Seq.take 8 |> Seq.toList
// [1; 1; 2; 6; 24; 120; 720; 5040]