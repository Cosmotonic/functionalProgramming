
// 1.1 
let rec take ((n:int),(xs:'a list)) : 'a list = 
    match xs with 
    | [] -> raise( System.Exception "Take: not enough elements")
    | x::rest -> if n > 0 then x::take ((n-1), rest) 
                 else rest

take (3, [1;2;4;5])

let partition (p:'a-> bool) (xs:'a list) : 'a list * 'a list = 
    let rec aux acc p xs = 
        match xs with 
        | [] -> acc 
        | x::rest ->
                let (higher,lower) = acc 
                if (p x) then aux (x::higher,lower) p rest else aux (higher,x::lower) p rest    
    aux ([],[]) p xs 

partition ((<)5) [1;2;5;3;5;9;9;10] 


let rec isSorted (xs:'a list) : bool when 'a : comparison = 
    match xs with 
    | [] -> true 
    | x::y::rest -> if x <= y then isSorted (y::rest) else false 
    | x -> true 

isSorted  [1;2;5;3;5;9;9;10] 
isSorted  [1;2;5;] 


// 1.2 
let rec tri n =
    if n = 0
        then 0
        else if n = 1 || n = 2
            then 1
            else tri (n - 1) +
                    tri (n - 2) +
                    tri (n - 3)

let natSeq = Seq.initInfinite (fun n -> n ) 
Seq.take 4 natSeq 

let triSeq = Seq.initInfinite (tri) 
Seq.take 4 triSeq
let triSeq' = Seq.cache triSeq  



// 2 
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

let org2 =
    Node(("Kurt","CEO" ),
        [Node(("Hanne","CFO"),
                [Node(("Ulla","Accountant"),[])]);
        Node(("Hans","CTO"),
            [Node(("Kurt","Manager"),
                [Node (("PIa", "Developer"),[])])])])


let numValues (rt:RoseTree<'a>) : int = 
    let rec aux rt =  
        match rt with 
        | Node (_, children ) ->  1 + List.sumBy aux children 
    aux rt 

numValues org

let rec layers (rt:RoseTree<'a>) : int =    
    match rt with 
    | Leaf _ -> 1
    | Node (_,[]) -> 1
    | Node (_,children) ->
               1 + ( children |> List.map layers |> List.max 
)

let rec layers1 (rt:RoseTree<'a>) : int =
    match rt with
    | Leaf _ -> 1
    | Node (_,[]) -> 1
    | Node (_,children) ->

        let rec maxLayer xs =
            match xs with
            | [x] -> layers x
            | x::rest ->
                let m = maxLayer rest
                if layers x > m then layers x else m
            | _ -> 0 

        1 + maxLayer children

let rec chkRoseTree (rt:RoseTree<'a>) : bool =  
    match rt with 
    | Leaf _ -> true 
    | Node (_,[]) -> false 
    | Node (_,children) -> 
                        children |> List.forall chkRoseTree 

chkRoseTree org

Node(("Hanne", "CEO"),
    [Node(("Bob", "CTO"),
        [Leaf ("Peter", "Developer"); 
        Leaf ("Ulla", "Developer")]);
    Node (("Kirsten", "CFO"),
        [Leaf ("Hans", "Accountant")])])

let rec fixRoseTree (rt:RoseTree<'a>) : RoseTree<'a> = 
    match rt with 
    | Leaf x -> Leaf x
    | Node (x,[]) -> Leaf x
    | Node (x, children) -> 
                        let fixChildren = List.map fixRoseTree children 
                        Node (x, fixChildren) 
fixRoseTree org 


// 3.1. 
type 'a Zipper = 'a list * 'a * 'a list // left, focus, right 

let ex1 = ([2;1],3,[4;5;6])
let ex2 = ([],'a',['b';'c';'d';'e';'f'])


let ex3 = (["D", "C", "B", "A"],"D",[])
let ex = ([(1,'a')], (1,'b'), [(3,'c')])

// They are polymorphic 


let numElems (z:'a Zipper) : int = 
    let (left, mid, right) = z 
    let total = left @ right 
    List.length total + 1

numElems ex1


let peekFocus (z:'a Zipper) : 'a = 
    let (left, mid, right) = z
    mid 

peekFocus ex2 

let peekRight (z:'a Zipper) : 'a option = 
    let (left, mid, right) = z
    match right with 
    | [] -> None  
    | x::rest -> Some x 

peekRight ex2 


let insertAfter (e:'a) (z:'a Zipper) : 'a Zipper = 
    let (left, mid, right) = z
    (left, mid, e::right) 

peekRight (insertAfter 42 ex1) 

let moveLeft (z:'a Zipper) : 'a Zipper = 
    let (left, mid, right) = z
    match left with 
    | [] -> (left, mid, right) 
    | x::rest -> (rest, x, mid::right )     

moveLeft ex1 


let rec existsLeft (p:'a -> bool) (z:'a Zipper) : bool = 
    let (left, mid, right) = z
    match left with 
    | [] -> false 
    | x::rest -> if (p x ) then true else (existsLeft p (rest, mid, right))
existsLeft ((=) 1) ex1 



let moveLeftCircular (z:'a Zipper) : 'a Zipper = 
    let (left, mid, right) = z  // [] a [letters;;] 
    match left with 
    | [] -> 
            let newMid = List.last right 
            let newLeft = (mid :: right |> (List.removeAt (List.length right) ) |>  List.rev )
            (newLeft, newMid, []  )

    | x::rest -> (rest, x, mid::right )
    
moveLeftCircular ex2 // evaluates to (['e';'d';'c';'b';'a'],'f',[]).


let map (f:'a->'b) (z:'a Zipper) : 'b Zipper = 
    let (left, mid, right ) = z 
    let leftZip = List.map (fun x -> f x) left 
    let rightZip  = List.map (fun x -> f x) left 
    let newMid = f mid 
    (leftZip, newMid, rightZip) 

map string ex1 


let foldLeft (f:'b->'a->'b) (e:'b) (z:'a Zipper) : 'b  =
    let (left, mid, right ) = z 
    mid::left |> List.fold (fun acc x -> (f acc x) ) e 

foldLeft (+) 0 ex1 // ((0 + 3) + 2) + 1
foldLeft (-) 0 ex1 // ((0 - 3) - 2) - 1



// 4.1 
let rec delannoy ((m:int), (n:int)) : int = 
    if n=0 || m=0 then 1 
     else delannoy(m-1,n) + delannoy(m, n-1) + delannoy(m-1, n-1) 
     
delannoy (5,5)

// It is not tail recursive as the last thing that is being done is addition:  a + b + c


// 4.2 -- NOT done by me. 
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

// With cache each pair is computed at maximum once. 


// 4.3 

let genMNs (n:int) : (int*int) list = 
    [ for m in 1..n do
        for t in 1..n do
            yield (m,t) ]

genMNs 4 

let tmp = [(1,1);(1,2);(2,1);(2,2)]

let delannoyXS (xs:(int*int) list)  : int list = 
    let rec aux xs =  
        match xs with 
        | [] -> [] 
        | (x,y)::rest -> delannoy (x,y)::aux rest 
    aux xs 

delannoyXS tmp 