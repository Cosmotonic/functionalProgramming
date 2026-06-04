

let rec tryFind p = function
        | x::xs when p x-> Some x
        | _::xs-> tryFind p xs
        | _-> None ;;


// -- HIGH ORDER FUNCTION -- 
// higher order function is a function that takes a function as an argument
let add x y = x + y
let applyTwice f x = f (f x)
applyTwice (add 5) 10
// Result: 20

let rec foldBack f xlst e =
        match xlst with
        | x::xs-> f x (foldBack f xs e)
        | []-> e ;;

// Så bliver f = (fun x acc -> x + acc)
let sum = foldBack (fun x acc -> x + acc) [1; 2; 3] 0
let concat = foldBack (fun x acc -> x + " " + acc) ["hello"; "world"; "!"] ""



// Binary trees 
type 'a BinTree =
    | Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

let intBinTree =
    Node(43, Node(25, Node(56, Leaf, Leaf), Leaf), 
         Node(562, Leaf, Node(78, Leaf, Leaf)))


let rec countNodes tree =
    match tree with
    | Leaf -> (1, 0)
    | Node(_, treeL, treeR) ->
        let (ll, lr) = countNodes treeL in
        let (rl, rr) = countNodes treeR in
        (ll + rl, 1 + lr + rr)

countNodes intBinTree;;

let rec foldBack f xlst e =
        match xlst with
        | x::xs-> f x (foldBack f xs e)
        | []-> e ;;
