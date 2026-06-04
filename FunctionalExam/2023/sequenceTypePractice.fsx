

// Question 1. 

type TwoLists = {listX: int list;
                 listY: int list}

let rec f {listX = lX; listY = lY} =
    match (lX, lY) with
    ([], [])-> f {listX=[]; listY=[]}
    | ([], y::ys)-> f {listX=[y]; listY = ys}
    | (x::xs, [])-> {listX=x::xs; listY = []}
    | (xs, y::ys)-> f {listX=y::xs; listY = ys}

// 1.1.1 
// The function stacks evertyhing from 1Y onto 1X until list 1Y is exhausted. 

// 1.1.2 
// The initial match ([], [])-> f {listX=[]; listY=[]} runs infitley until stopped manually. 

// 1.1.3
// The input values where 1X has elements and 1Y is exhausted terminates the function. (x::xs, []) 

// 1.2.1. 
let rec f2 {listX = lX; listY = lY} =
    match (lX, lY) with
    ([], [])-> {listX=[]; listY=[]}
    | ([], y::ys)-> f {listX=[y]; listY = ys}
    | (x::xs, [])-> {listX=x::xs; listY = []}
    | (xs, y::ys)-> f {listX=y::xs; listY = ys}

f2 {listX=[]; listY=[]}

// 1.2.2 
let rec f3 {listX = lX; listY = lY} =
    match (lX, lY) with
    | (xs, [])-> {listX=xs; listY = []}
    | (xs, y::ys)-> f {listX=y::xs; listY = ys}


// 1.3.1 
let infSeq = Seq.initInfinite id

let mySeq : seq<int * int> = 
    Seq.map (fun i -> (i, -i)) infSeq


let mySeq1 = seq { 1; 2; 3 }           // simple sequence
let mySeq2 = seq { 1..10 }             // range sequence
let mySeq3 = seq { 1..2..10 }          // range with step

let mySeq4 = seq { 1..100 }               // infinite sequence!



let infSeq = Seq.initInfinite id // fun e -> (e, -e) ) // id


let item = infSeq |> Seq.map (fun e -> (e, -e) ) |> Seq.take 4  
let item = infSeq |> Seq.map (fun e -> (e, -e) ) |> Seq.take 10  

let item3 = Seq.map (fun e -> (e, -e))  (Seq.take 10 infSeq ) 
    // sent inf seq |> to map with a condition/predicate |> return first four elements. 



let fizzBuzz n =
    if n % 3 = 0 && n % 5 = 0 then
        "fizzbuzz"
    elif n % 3 = 0 then
        "fizz"
    elif n % 5 = 0 then
        "buzz"
    else
        string n

fizzBuzz 15

let fizzBuzzSeq =
    seq {
        for n in 0 .. System.Int32.MaxValue do 
            // yield fizzBuzz n //  eg. seq {  yield 1 yield 2 yield 3 } = seq [1;2;3]
            yield! [fizzBuzz n; fizzBuzz n] 
            // yield bang explodes the [] and just takes everyting one at a time and add it to the return seq[]
            // yield takes the entire element and returns it [[1;3][5;6]]
    }
Seq.take 5 fizzBuzzSeq // yield [["fizzbuzz"; "fizzbuzz"]; ["1"; "1"]; ["2"; "2"]; ["fizz"; "fizz"]; ...]
Seq.take 5 fizzBuzzSeq // yield! ["fizzbuzz"; "fizzbuzz"; "1"; "1"; ...]


let fizz = seq {
        yield! [ [1;3;3];[5] ]
}
Seq.take 3 fizz
Seq.take 1 fizz 


let ema : list<seq<int>> = [
    seq {1;2}; 
    seq {3;4}
]

let tra = seq{
    for n in ema do 
        yield! n
}
tra |> Seq.map fizzBuzz 









type car = 
    | Bmw of string 
    | Vw of string 
    | Volvo of string 
type parkinglot = 
    | Location of string * car list
    | House of string 
// copenhagen 2 bmw 3 volvo 

let bmwE3 = Bmw "E3"
let VwEtron = Vw "Etron"

let parking : parkinglot = Location ("Copenhagen",[VwEtron;bmwE3])


type Field = ValField of int
type FieldMap = Map<string, Field>

let kValue : Field = ValField 10

let mapOfFields : FieldMap  = Map.empty
mapOfFields.Add ("kas",(kValue))


type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>


let emptyHp1 = EmptyHP
let emptyHp2 = EmptyHP

let hp = HP (1, HP(2, EmptyHP, EmptyHP), HP(3, EmptyHP, emptyHp1)) 


type 'a diffList =
    LISTS of 'a list list
    | SEQ of 'a diffList * 'a diffList


let diffLst : 'a diffList = LISTS [[1;3;4];[13;12]]
let seq1 : int diffList = SEQ (diffLst, LISTS[])

let exDF01 = SEQ (LISTS [[1;2];[3;4]],
                SEQ (LISTS [[];[5]],LISTS [[6;7];[8]]))
let exDF02 = SEQ (LISTS [[1;3];[2;4]],
                SEQ (LISTS [[];[5]],LISTS [[6;7];[8]]))

exDF01 = exDF02

// Declare an F# function mkDiffList of type unit-> ’a diffList that evaluates to an
// empty difference list. For instance: // tag type TYPE list of lists 
let mkDiffList : 'a diffList = LISTS[]