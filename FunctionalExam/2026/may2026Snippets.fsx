// Functional Programming
// Friday May 22, 2026

// Remember insert declaration on your hand-in.

// I hereby declare that I myself have created this exam hand in in its entirety without help from AI
// tooling and anybody else.

// *** QUESTION 1
// * Question 1.1 
// 1.1.1 - take function that returns N elements of a list. DONT USE LIBRARY FUNCTONIS. 
// return a list with only the n elements of the given list. 
let rec take (n:int, xs:'a list) : 'a list = 
    match n, xs with 
    | 0, _ -> []
    | _, [] -> failwith "Take: not enough elements" 
    | n, h::tail -> h :: take (n-1, tail)

take (3,[1;2;4;5])





// 1.1.2 partition ((<)5) [1;2;5;3;5;9;9;10] - separate the list in two lists that meets the predicat. (solve tail recursively )
let rec partition (p:'a->bool) (xs:'a list) : 'a list * 'a list = 
    match xs with
    | [] -> ([], [])
    | h::t -> 
        let (higher, lower) = partition p t // de tolister 
        if (p h) then (h::higher, lower) else (higher, h::lower)

partition ((<)5) [1;2;5;3;5;9;9;10]
// 1.1.3 partitionA - 
// make it tail recursive - attempted on previous step. )


// 1.1.4
let rec isSorted (xs:'a list) : bool = 
    match xs with 
    | [] -> true // meaning if nothing is left to compare we must be true. 
    | h1::h2::tail -> 
              if h1 > h2 then isSorted ( h2::tail ) else false //  all prevoius are bigger then true else false. 

isSorted [1;2;3] 
isSorted [2;1]

// * Question 1.2

let rec tri n =
  if n = 0
    then 0
    else if n = 1 || n = 2
      then 1
      else tri (n - 1) +
           tri (n - 2) +
           tri (n - 3)
let natSeq = Seq.initInfinite id
Seq.take 4 natSeq // val it: int seq = seq [0; 1; 1; 2; ...]

// triSeq'
let triSeq =
    seq {
        for n in 0 .. System.Int32.MaxValue do 
            // yield fizzBuzz n //  eg. seq {  yield 1 yield 2 yield 3 } = seq [1;2;3]
            yield tri n 
            // yield bang explodes the [] and just takes everyting one at a time and add it to the return seq[]
            // yield takes the entire element and returns it [[1;3][5;6]]
    }

Seq.take 10 triSeq // val it: int seq = seq [0; 1; 1; 2; ...]

let triSeq_cached =
    seq {
        for n in 0 .. System.Int32.MaxValue do 
            // yield fizzBuzz n //  eg. seq {  yield 1 yield 2 yield 3 } = seq [1;2;3]
            yield tri n 
            // yield bang explodes the [] and just takes everyting one at a time and add it to the return seq[]
            // yield takes the entire element and returns it [[1;3][5;6]]
    }
Seq.take 10 triSeq
let triNumC : seq<int> = Seq.cache triSeq // val it: int seq = seq [0; 1; 1; 2; ...]




// *** QUESTION 2
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

// Hanne (CEO)
//   |- Bob (CTO)
//   |   |- Peter (Developer)
//   |   |- Ulla (Developer)
//   |- Kirsten (CFO)
//       |- Hans (Annountant)

// * Question 2.1
// 2.1.1 org2
let org2= 
   Node(("Kurt","CEO" ),
       [ Node(("Hanne","CFO"),
              [Node (("Ulla","Accountant"),[])]);
         Node(("Kirsten","CFO"),
              [Node(("Peter","Developer"),[
                  Node(("Ulla","Developer"),[])]);])])
               
// 2.1.2 numValues org
let numValues (rt:RoseTree<'a>) : int = 
    let rec aux r acc =  
        match r with 
        | Leaf _ -> acc 
        | Node (t,lst) ->  (aux r acc) + ( acc + 1 )
    aux rt 0 

numValues org // evaluates to 6.




// 2.1.3 layers org - I want to find the deepest layer and return it. 
let layers (rt:RoseTree<'a>) : int =
    let rec matchList r accDepth =  
        match r with 
        | Leaf (_,_) -> accDepth + 1 
        | Node (t, list) -> 
                    match list with 
                    | [] -> accDepth 
                    | Node (t, nList) -> 
    aux r 0 

layers org // 


// * Question 2.2
Node(("Hanne", "CEO"),
     [Node(("Bob", "CTO"),
           [Leaf ("Peter", "Developer"); Leaf ("Ulla", "Developer")]);
      Node (("Kirsten", "CFO"),
            [Leaf ("Hans", "Accountant")])])
// 2.2.1 chkRoseTree org  -- If there exists empty lists then return false. 

// if no nodes have empty lists then its true. // return here 
let chkRoseTree (rt:RoseTree<'a>) : bool = 
    let rec matchList rt acc =  
        match rt with 
        | Leaf (_,_) -> true
        | Node (t, list) -> 
                    match list with 
                    | [] -> false 
    matchList rt 0 
chkRoseTree org

// // fixRoseTree org
// Node((("Hanne", "CEO"), 5),
//      [Node((("Bob", "CTO"), 2),
//            [Node ((("Peter", "Developer"), 0), []);
//             Node ((("Ulla", "Developer"), 0), [])]);
//       Node ((("Kirsten", "CFO"), 1),
//             [Node ((("Hans", "Accountant"), 0), [])])])
// 
//  fixRoseTree rt of type RoseTree<’a>-> RoseTree<’a>,






// *** QUESTION 3

type 'a Zipper = 'a list * 'a * 'a list // left, focus, right

let ex1 : 'a Zipper  = ([2;1],3,[4;5;6])
let ex2 : 'a Zipper  = ([],'a',['b';'c';'d';'e';'f'])

// * Question 3.1

// 3.1.1
let ex3 = (["A";"B";"C"],"D",[])
let ex4 = ([(1,'a')], (2,'b'), [(3,'c')])

// 3.1.2 The types are polymorphic lists indicated with 'a 

// numElems ex1
let numElems (z:'a Zipper) : int = 
    match z with 
    | (listL,focus,listR) -> List.length listR + 1 + List.length listL
numElems ex1 // type 'a

// peekFocus ex2
let peekFocus (z:'a Zipper) : 'a = 
    match z with 
    | (listR,focus,listL) -> focus 
peekFocus ex1 // val it: int = 3

// peekRight ex2
let peekRight (z: 'a Zipper) : char = 
    match z with 
    | (listL,focus,listR) -> match listR with 
                              | x::tail -> x
peekRight ex2 // type 'a

// * Question 3.2

// 3.2.1 
let insertAfter (e:'a) (z:'a Zipper) : 'a Zipper = 
    match z with 
    | (listL, focus, listR) -> 
                        let newRight = e::listR
                        (listL, focus, newRight)

// insertAfter 'c' ex1
// peekRight (insertAfter '42' ex1) = Some 42
// 
// moveLeft ex1
let moveLeft (e:'a) (z:'a Zipper) : 'a Zipper = 
    match z with 
    | (listL, focus, listR) -> match listL with 
                                | [] ->  (listL, focus, listR)
                                | x::rest -> 
                                            let newFocus = x 
                                            let newLeft = rest     
                                            (newLeft, newFocus, listR)
moveLeft ex1

// existsLeft ((=)1) ex1
let existsLeft (p:'a-> bool) (z:'a Zipper) : bool = 
    match z with 
    | (listL, focus, listR) -> match listL with 
                                | [] -> false 
                                | x::rest -> p x = true then true else false 

// moveLeftCircular ex2

// ****** moving to four return here in case of time. 

// * Question 3.3

// map string ex1
let map  (f:'a-> 'b) (z:'a Zipper)
        // here i would just run the map List.Map on all elements like so: 
        let mappedList= List.map f x.elems // to exchange  all elements in each list. 


// foldLeft (+) 0 ex1
let foldLeft f e z = 
    match z with 
    | (listL, focus, listR) -> match listL with 
                                let newLeft = List.rev (List.fold  (fun acc e -> f e ) [] listL )
                                (newLeft, focus, listR)

    

foldLeft ex1

// foldLeft (-) 0 ex1








// *** QUESTION 4

// * Question 4.1

// delannoy (5,5)

let rec delannoy ((m:int), (n:int)) : int = 
    if m = 0 || n = 0
    then 1 
    else delannoy(m - 1,n)+delannoy(m,n- 1)+delannoy(m- 1,n- 1) 

delannoy (5,5)

// The function is recursive. The last thing we do is call Delannoy. 


let delannoyCache (m,n) =
  let cache = Map.empty
  let c = System.Collections.Generic.Dictionary<int*int,int>()
  let rec delannoy' (m,n) =
    match c.TryGetValue((m,n)) with
        (true, v) -> v
      | _ ->
        if m = 0 || n = 0
        then 1
        else let r1 = delannoy' (m-1,n)
             let r2 = delannoy' (m,n-1)
             let r3 = delannoy' (m-1,n-1)
             let r = r1+r2+r3
             c.Add((m,n),r)
             r
  delannoy' (m,n)
delannoyCache (5,5)
 
 
// We use caches so elements aren't recomputed

// * Question 4.3

// genMNs 2 
let genMNs n : seq<int*int>   = 
    seq {
        for i in 1..n do 
            for j in 1..n do 
                yield! [i,j]
        }

let NrList = Seq.toList (Seq.take 4 (genMNs 2))

// delannoyXS (genMNs 2) 
let delannoyXS (xs) : int list = 
    let rec mkList acc xs = 
        match xs with 
        | [] -> acc
        | x -> acc
        | x::rest -> match x with 
                        | (y,z) ->  delannoy (y,z)::mkList acc rest
    
    mkList [] xs 

delannoyXS NrList


 

