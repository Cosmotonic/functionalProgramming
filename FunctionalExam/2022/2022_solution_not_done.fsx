
type TrashItem<'a> =
    | Paper of string
    | Glass of string
    | Other of 'a

let item1 = Other ("Shirt", "Clothes")
let item2 = Paper "Newspaper"
let item3 = Other ("Sneakers", "Shoe")
let item4 = Glass "Wine glass"

// Quesetion 1.1 

// What are the types of item1 and item2. Explain your answer? 
// Answer: Item1: TrashItem<string * string>
//         Item2: TrashItem<'a'>

// Is the type TrashItem monomorphic or polymorphic?                
// Answer: Polymorphic

// Declare an F# value items of type: TrashItem<string*string> list
let items = [Paper "Magazine"; Glass "Bottle"; Glass "Jam"; Other ("Beer can", "Aluminium"); Other ("bag", "Plastic")]  

(*
Declare an F# function 

ppTrashItem fnPP item of type (’a -> string) -> TrashItem<’a> -> string 

that returns a string representing the printed trash item item. The argument
function fnPP is used to format items of category Other, i.e., to use ppTrashItem you also
need to define an argument function fnPP. The result string must have the format as shown below.
You may use the template below.
*)

//  string concatenation (sammensætning af strenge) Printing
let fnPP (n, t) = 
    $"{t} ({n})"

let ppTrashItem fnPP (item : TrashItem<'a>) = 
    match item with 
    | Paper name -> $"Paper ({name})" 
    | Glass name -> $"Glass ({name})" 
    | Other v -> fnPP v 
 
let outputString = ppTrashItem fnPP item2 
printfn "%s" outputString; 

// Declare an F# function isPaper item of type TrashItem<’a> -> bool that returns true if
// item is of category paper; otherwise returns false. For instance isPaper item1 returns false
// and isPaper item2 returns true.

let isPaper (item : TrashItem<'a>) : bool = 
    match item with 
    | Paper name -> true 
    | _ -> false


// Question 1.2
type TrashCan<'a> =
    | Empty
    | TrashItems of TrashItem<'a> * TrashCan<'a>

// Declare an F# function addItem item tc of type TrashItem<’a> -> TrashCan<’a> -> TrashCan<’a>
// that adds trash item item to the trash can tc. For instance, addItem item1 
// Empty returns the value TrashItems(Other("Shirt","Clothes"),Empty).

let addItem item tc = 
    TrashItems(item, tc)

let tc1 = addItem item1 Empty   // Indeholder: [item1; Empty]
let tc2 = addItem item2 tc1     // Indeholder: [item2; item1; Empty]
let tc3 = addItem item3 tc2     // Indeholder: [item3; item2; item1; Empty]
let tc = Empty |> addItem item1 |> addItem item2

// Declare an F# function ofList ts of type TrashItem<'a> list -> TrashCan<'a> that
// returns a trash can given a list of trash items. For instance, declaring

let rec ofList ts =
    match ts with
    | [] -> Empty
    | head :: tail -> TrashItems(head, ofList tail)

let tcEx = ofList [item2; item1; item3]

// Declarean F# function forAll fnP tc of type(TrashItem<’a>->bool)->TrashCan<’a>->bool that returns true 
// if predicate function fnP is true for all trash items in trash can tc; other wise returns false. 
// For instance forAll isPaper Empty returns true and forAll isPaper tcEx returns false

// fnP er en forkortelse for function Predicate (funktion prædikat). Her bruger jeg min funktion isPaper 
// for at tjekke om alti papirkurven er papir. 
let rec forAll fnP tc =
    match tc with
    | Empty -> true
    | TrashItems(item, rest) -> fnP item && forAll fnP rest // in isSorted we run this func, where we use partial application. 

forAll isPaper tcEx

(* Declare an F# function isSameCategory item1 item2 of type TrashItem<’a>->TrashItem<’b>-> bool 
that returns true if the two trash items are of same category; otherwise returns false.
For instance, isSameCategory item1 item2 returns false and isSameCategory item1
item3 returns true. *) // Compare items. 

let isSameCategory a b : bool = 
    match (a, b) with 
    | Paper _, Paper _ -> true
    | Glass _, Glass _ -> true 
    | Other _, Other _ -> true 
    | _ -> false 

(* Declare an F# function isSorted tc of type TrashCan<’a>-> bool that returns true if all
items in trash can tc are of same catetory. For instance isSorted tcEx returns false. You must
use the functions forAll and isSameCategory to get full points *) // Partial Application

let isSorted tc =
    match tc with
    | Empty -> true
    | TrashItems(head, rest) -> forAll (isSameCategory head) rest // partial application, we already give isSameCategory the first input here. 
                                                                  // in other words: isSameCatergory bliver vores FnP og det første input til fnp. I vores 
                                                                  // I for all har vi "| ... -> fnP item && ..." der inputter vi første item af "rest"
                                                                  // Derfor er det et partial application. 
let tcExample = ofList [item1; item1; item1]
isSorted tcExample

// note om functional predicate eksempel.
// 1. En funktion der skal bruge TO tal -  
let add x y = x + y
// 2. Vi giver den kun det ENE (Partial Application)
let addFive = add 5
// 3. Nu er addFive en funktion der mangler ET tal
let result = addFive 10 // result bliver 15


// Question 1.3
// Declare an F# function fold f e tc of type (’a-> TrashItem<’b>-> ’a)-> ’a-> rashCan<’b>-> ’a 
// Tthat folds function f over items in trash can tc using initial accumulator
// value e. For instance, fold (fun n _-> n+1) 0 tcEx returns 5, i.e., the number of items in
// tcEx.

let rec fold f e tc = 
    match tc with 
    | Empty -> e
    | TrashItems(item, rest) -> fold f (f e item) rest 


fold  (fun acc _ -> acc + 1) 0 tcExample // start counter from 0