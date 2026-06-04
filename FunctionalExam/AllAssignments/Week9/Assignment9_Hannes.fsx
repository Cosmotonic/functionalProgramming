// testing to get into trees. Not assignment. 

type BinTree<'a> =  |Leaf
                    | Node of BinTree<'a> * 'a * BinTree<'a>

let rec preOrder = function
    | Leaf -> []
    | Node(tl,x,tr) -> x :: (preOrder tl) @ (preOrder tr)


let t3 = Node(Node(Leaf, -3, Leaf), 0, Node(Leaf, 2, Leaf));;
let t4 = Node(t3, 5, Node(Leaf, 7, Leaf));;

preOrder t3

// 9.8
// Develop a version of the counting function for binary trees
//         countA: int-> BinTree<’a>-> int
// that makes use of an accumulating parameter. Observe that this function is not tail recursive.

let t5 = Node(Node(Leaf, 7, Leaf), 5, Node(Leaf, 7, Leaf));;

let rec count = function
    | Leaf -> 0
    | Node(tl,x, tr) -> 1 + count(tl) + count(tr)

count t5

let rec countA = function
    | acc, Leaf -> acc
    | acc, Node(tl,x,tr) ->
        let acc1 = acc + 1
        let acc2 = countA(acc1, tl) 
        countA(acc2, tr)

countA (0, t5)

// 9.9
let rec countAC (t: BinTree<'a>) (acc: int) (cont: int -> 'b) :'b =
    match t with
    | Leaf -> cont acc
    | Node(tl, x, tr) ->
        countAC tl (acc + 1) (fun accAfterLeft ->
            countAC tr accAfterLeft cont)

countAC t5 0 (fun x -> printfn "Total nodes: %d" x; x)


// 9.10
let rec bigListK n k =
    if n=0 then k []
    else bigListK (n-1) (fun res -> 1::k(res))

bigListK 13000 id

(*Since we keep taking one from the list and recursivly calling the tail we 
  build up huge list which finally results in a stack overflow. *)

// 9.11
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
countA (0, t6)

countAC t6 0 (fun x -> x)