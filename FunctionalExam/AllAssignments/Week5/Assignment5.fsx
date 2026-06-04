
// 5.1 
type 'a BinTree =
    | Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

let intBinTree= Node(43,Node(25,Node(56,Leaf,Leaf),Node(500, Leaf, Leaf)),Node(562,Node(60, Leaf, Leaf),Node(78,Leaf,Leaf)))

let rec inOrder (tree: 'a BinTree) : 'a list =
    match tree with
    | Leaf -> []
    | Node(x, left, right) -> inOrder left @ [x] @ inOrder right

inOrder intBinTree

// notes. 
type Color = 
    | Red 
    | Blue 
    | Green 

let describe (c: Color) =
    match c with
    | Red -> "det er rødt"
    | Blue -> "det er blåt"
    | Green -> "det er grønt"

describe Red

type Person =
    | Child of string
    | Adult of string * int

let age (p: Person) =
    match p with
    | Child(name) -> name + " er et barn"
    | Adult(name, age) -> name + " er " + age.ToString()    


age (Child "kasper");;
// Result: "kasper er barn"

age (Adult("kasper", 25));;
// Result: "kasper er 25"


// Exercise 5.2 Write a function 
// mapInOrder : (’a-> ’b)->’aBinTree->’bBinTree
type 'a BinaryTree =
    | Leaf
    | Node of 'a * 'a BinaryTree * 'a BinaryTree                            
let intBinTree1= Node(43,Node(25,Node(56,Leaf,Leaf),Leaf),Node(562,Node(60, Leaf, Leaf),Node(78,Leaf,Leaf)))


let rec mapInOrder (f: 'a -> 'b) (tree: 'a BinaryTree) : 'b BinaryTree = 
    match tree with 
    | Leaf -> Leaf // hvis tree inputtet er et leaf så stop. 
    | Node(x, left, right) -> Node(f x, mapInOrder f left, mapInOrder f right)
    // hvis det er en node med input f, left, right 
    // så hvad? havd sker der erfter ->

mapInOrder (fun x -> x + 1) intBinTree1;


let rec mapPostOrder (f: 'a -> 'b ) (tree: 'a BinaryTree) : 'b BinaryTree =
    match tree with 
    | Leaf -> Leaf
    | Node(funcInput, left, right) -> Node(f funcInput, mapPostOrder f left, mapPostOrder f right)

let testTree = Node(10, Node(5, Leaf, Leaf), Node(15, Leaf, Leaf))

mapPostOrder (fun x -> x * 2) testTree



// 5.3 // in-order (venstre -> node -> højre); post-order (venstre -> højre -> node) 
let rec foldInOrder (f: 'a -> 'b -> 'b) (initValue: 'b) (tree: 'a BinaryTree) : 'b = 
    match tree with
    | Leaf -> initValue  // 0, 0.0, " " "Hvis det matcher Leaf - der er intet (slut)."
    | Node(funcInput, left, right) -> // "Hvis det matcher Node - der er tre ting: x, left, right."
        let leftResult = foldInOrder f initValue left // gem alt left i left result 
        let nodeResult = f funcInput leftResult // input value, left tree, right tree
        foldInOrder f nodeResult right
        // f = a + n 
  
// foldInOrder : (’a-> ’b-> ’b)-> ’b-> ’a BinTree-> ’b 
let floatBinaryTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf), Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))
let stringBinaryTree = Node("s",Node("a", Node("k",Leaf, Leaf), Leaf), Node("p", Leaf, Node("e", Leaf,Leaf)))

foldInOrder (fun n a -> a + n) 0.0 floatBinaryTree
foldInOrder (fun n a -> a + n) " " stringBinaryTree


// Ekstra LLM opgave. 
type 'a BinaryTree1 =
    | Leaf
    | Node of 'a * 'a BinaryTree1 * 'a BinaryTree1

let floatBinaryTree1 = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf), Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

let rec foldPostOrder (f: 'a -> 'b -> 'b ) (unit: 'b ) (tree: 'a BinaryTree1) : 'b = 
    match tree with
    | Leaf -> unit
    | Node (x, left, right) -> 
        let leftResult  = foldPostOrder f unit left
        let rightResult = foldPostOrder f unit right
        f x rightResult

foldPostOrder (fun n a -> a + n) 0.0 floatBinaryTree1