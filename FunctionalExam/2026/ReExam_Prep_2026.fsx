
// 1.1.1 
let rec takeN (n:int, xs:'a list) : 'a list = 
    match n, xs with 
    | 0, _ -> []
    | _, [] -> failwith "Take: not enough elements" 
    | n, h::t -> h :: takeN (n-1, t) 

takeN (3,[4;2;1;4])


// 1.1.2 
let rec partition (p:'a->bool) (xs:'a list) : 'a list * 'a list = 
    match xs with
    | [] -> ([], [])
    | h::t -> 
        let (higher, lower) = partition p t // de tolister + rec kald inden vi er færdige
        if (p h) then (h::higher, lower) else (higher, h::lower)

partition ((<)5) [1;2;5;3;5;9;9;10]

// 1.1.3
let rec partition1 (p:'a->bool) (xs:'a list) : 'a list * 'a list = 
    let rec loop xs higher lower = 
        match xs with 
        | [] -> (higher, lower)
        | h::t -> if (p h) then loop t  (h::higher) lower 
                            else loop t  higher (h::lower)
    loop xs  [] []

partition1 ((<)5) [1;2;5;3;5;9;9;10]

// 1.1.4 
let rec isSorted (xs:'a list) : bool = 
    match xs with 
    | [] -> true 
    | [_] -> true 
    | h::m::t -> 
        let pNr = h 
        if m > pNr then isSorted (m::t) 
        else false 

isSorted [1;3;2]
isSorted [4;3;]


// 1.2
let rec tri n =
    if n = 0
        then 0
        else if n = 1 || n = 2
            then 1
            else tri (n - 1) +
                    tri (n - 2) +
                    tri (n - 3)

// 1.2.1
let natSeq = Seq.initInfinite id
Seq.take 4 natSeq

// 1.2.2
let triSeq = 
    seq{
        for n in natSeq do // 0 .. System.Int32.MaxValue do 
            yield tri n 
    }

Seq.take 4 triSeq

// 1.2.3. 
let triSeqCache = Seq.cache triSeq



// 2.1 
type RoseTree<'T> =
    | Node of 'T * RoseTree<'T> list
    | Leaf of 'T

type Employee = string * string // (Name, Title)

let org =
    Node(("Hanne","CEO" ),
        [ Node(("Bob","CTO"),
            [Node(("Peter","Developer"),[]);
            Node(("Ulla","Developer"),[])]);
        Node(("Kirsten","CFO"),
            [Node(("Hans","Accountant"),[])])])

// 2.1.1 
let org2 =
    Node(("Kurt","CEO" ),
        [ 
        Node(("Hanne","CFO"),
            [Node(("Ulla","Accountant"),[])]);
        Node(("Hans","CTO"), [
            Node(("Kurt","Manager"),[
                    Node(("Pia", "Developer"), [])])])]);


// 2.1.2 
let rec numValues (rt:RoseTree<'a>) : int = 
    match rt with 
    | Leaf v -> 0
    | Node (v,children) -> 1 + (children |> List.fold (fun s x -> s + numValues x ) 0)  

numValues org   

// 2.1.3 
let rec layers (rt:RoseTree<'a>) : int =
    match rt with
    | Leaf _ -> 1
    | Node(_, []) -> 1
    | Node(_, children) ->
        1 + (children |> List.map layers |> List.max) // layers PeterTræ = 1, layers UllaTræ = 1

layers org

// 2.2 
// 2.2.1 
let rec chkRoseTree (rt:RoseTree<'a>) : bool = 
    match rt with 
    | Leaf _ -> true 
    | Node (_,[]) -> false 
    | Node (_, children) ->  
                children |> List.forall chkRoseTree

chkRoseTree org

// 2.2.2 
let rec fixRoseTree (rt:RoseTree<'a>) : RoseTree<'a> = 
    match rt with 
    | Leaf v -> Leaf v  
    | Node (v,[]) -> Leaf v 
    | Node (v, children) -> 
        let fixedChildren = children |> List.map fixRoseTree // huske den bygger en recursive stack
        Node (v, fixedChildren)

fixRoseTree org

// 2.2.4 - Vanvid. 
let addNumSubTrees (rt:RoseTree<'a>) : RoseTree<'a * int> =
    let rec aux rt =
        match rt with
        | Leaf v ->  (Leaf (v, 0), 1)
        | Node(v, children) ->
            let annotatedChildren, sizes = children |> List.map aux |> List.unzip
            let totalNodes = 1 + List.sum sizes
            let numSubordinates = totalNodes - 1
            (Node((v, numSubordinates), annotatedChildren), totalNodes)
    fst (aux rt)

// 3.1. 
type 'a Zipper = 'a list * 'a * 'a list // left, focus, right
let ex1 = ([2;1],3,[4;5;6])
let ex2 = ([],'a',['b';'c';'d';'e';'f'])

// 3.1.1
let ex3  = (['A';'B';'C'],'D',[])
let ex4 = ([(1,'a')], (2,'b'), [(3,'c')])

// 3.1.3 
let numElemes (z:'a Zipper) : int = 
    let (a,b,c) = z
    let aL = List.length (a); 
    let cL = List.length (c); 
    aL+cL+1

numElemes  ex4

// 3.1.4 
let peekFocus (z:'a Zipper) : 'a = 
    let (a,b,c) = z 
    b 
peekFocus ex3 

// 3.1.5
let peekRight (z:'a Zipper) : 'a option = 
    let (a,b,c) = z 
    match c with
    | [] -> None 
    | h::t -> Some h 
peekRight ex3 

// 3.2

// 3.2.1 
let insertAfter (e:'a) (z:'a Zipper) : 'a Zipper = 
    let (a,b,c) = z 
    let newC = e::c
    (a,b,newC)

peekRight(insertAfter 42 ex1); 

// 3.2.2 
let moveLeft (z:'a Zipper) : 'a Zipper = 
    let (a,b,c) = z 
    match a with 
    | [] -> z 
    | h::t -> (t,h,b::c) 
moveLeft ex1

// 3.2.3
let existsLeft (p:'a->bool) (z:'a Zipper) : bool = 
    let (a,b,c) = z 
    List.exists (fun x -> p x ) a 
existsLeft ((=) 2)  ex1 


// 3.2.4 
let moveLeftCircular (z:'a Zipper) : 'a Zipper = 
    let (a,b,c) = z 
    match a with 
    | [] -> 
        let newFocus = List.last c
        let newList = b::(List.removeAt ((List.length c)-1) c)
        (List.rev newList, newFocus, [] )
    | h::t -> moveLeft z 
moveLeftCircular ex2 

// 3.2.
// 3.2.1 
let map (f:'a->'b) (z:'a Zipper) : 'b Zipper = 
    let (a, b, c) = z 
    let newA = a |> List.map f
    let newB = f b 
    let newC = c |> List.map f  
    (newA, newB, newC) 
map string ex1  

// 3.2.2 
let foldLeft (f:'b->'a->'b) (e:'b) (z:'a Zipper) : 'b = 
    let (a,b,c) = z 
    let foldList = (b::a) 
    List.foldBack (fun  x acc -> f acc x) foldList e  

foldLeft (+) 0 ex1
foldLeft (-) 0 ex1


// 4. 
// 4.1.1 
let rec delonnoy ((m:int), (n:int)) : int = 
    if n = 0 || m = 0 then 1 else 
    delonnoy(m-1,n) + delonnoy(m, n-1) + delonnoy(m-1, n-1); 
delonnoy (5,5)


// 4.2.1 
let delannoyCache2 (m,n) =
    let rec delannoy (m,n) cache =
        match Map.tryFind (m,n) cache with
        | Some v -> (v, cache)
        | None ->
            if m = 0 || n = 0 then
                (1, Map.add (m,n) 1 cache)
            else
                let (r1, cache1) = delannoy (m-1,n) cache
                let (r2, cache2) = delannoy (m,n-1) cache1
                let (r3, cache3) = delannoy (m-1,n-1) cache2

                let r = r1 + r2 + r3
                let cache4 = Map.add (m,n) r cache3

                (r, cache4) 
    fst (delannoy (m,n) Map.empty)
delannoyCache2 (5,5)


// 4.3.1 
// genMNs 2 
let genMNs (n:int) : (int*int) list  = 
    let myList = seq {
        for i in 1..n do 
            for j in 1..n do 
                yield! [i,j]
        }
    Seq.toList myList
genMNs 2 

let rec delannoyXS (xs:(int*int) list) : int list = 
    match xs with 
    | [] -> []
    | h::t -> delonnoy h :: delannoyXS t

delannoyXS (genMNs 2) 

