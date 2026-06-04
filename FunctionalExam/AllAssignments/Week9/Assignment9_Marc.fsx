// hand-in 9

// 9.8
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
//this function is not tail recursive because there are always depending a last call after 
//the function have counted the left and middle node, then it calls the right. the outercaller (countAcc right()) is waiting for the innercall countAcc left(acc + 1))

// 9.9
// Declare a tail-recursive functions with the type
// countAC : BinTree<’a>-> int-> (int-> ’b)-> ’b
// such that count t = countAC t 0id.The intuition with countAC tacis that a is the
// number of nodes being counted so far and c is the continuation.
let rec countAC (tree: binaryTree<'a>) (a) (c: int -> 'b) : 'b =
    match tree with 
    | Leaf -> c a // returns current total 
    | Node (left, _, right) ->
        countAC left (a+1) (fun leftCount -> 
            countAC right leftCount c)
//1. We hit a Node, so call countAC left (0+1) (fun leftResult -> countAC right leftResult c)
//2. Left is a Leaf, so we call the continuation with a=1 → starts counting right
//3. Right is a Leaf, so we call the original continuation c with 1
//4. c is id, so we just get back 1 

//exercise 9.3 (9.10 in book)
    // so the problem is not in the call bigListK 130000 id,  we continue bigListK (n-1) until we reaches 0.
    // when we go to 0 we call k[], that is 130000 nested functions we call and that is what causes the stack overflow.
    // it calls func 1 -> func 2 -> func 3 -> .... func 130000. each of those calls is waiting for the next to return and that results in the stack overflow.

//exercise 9.4 (9.11 in book)

//building leftTree that builds tree leaning to the left
let leftTree n=
    let rec aux i acc =
        if i = 0 then acc
        else aux(i-1) (Node(acc, i, Leaf))
    aux n Leaf

//building rightTree that builds tree leaning to the right
let rightTree n =
    let rec aux i acc =
        if i = 0 then acc
        else aux (i-1) (Node(Leaf, i, acc))
    aux n Leaf

//part 1: showing stacklimit when using count and countA
let t = leftTree 10
printfn "%d" (count t)
printfn "%d" (countA t 0)

//exercise 9.4 (11.1 in book)
let odds = Seq.initInfinite (fun i -> 2 * i + 1)
// i=0 → 1, i=1 → 3, i=2 → 5, etc.

//to see the first 10
odds |> Seq.take 10 |> Seq.toList
// [1; 3; 5; 7; 9; 11; 13; 15; 17; 19]

//exercise 9.5 (11.2 in book)
let factorials = Seq.initInfinite (fun i -> 
    let rec fact n = if n = 0 then 1 else n * fact(n-1)
    fact i)

//to see the first 8
factorials |> Seq.take 8 |> Seq.toList
// [1; 1; 2; 6; 24; 120; 720; 5040]