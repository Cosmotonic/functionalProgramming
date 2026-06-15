
type TrashItem<'a> = 
    Paper of string 
    | Glass of string 
    | Other of 'a 

let item1 = Other ("Shirt", "Clothes") 
let item2 = Paper "Newspaper"
let item3 = Other ("Sneakers", "shoes") 
let item4 = Glass "Wine glass" 

let items : list<TrashItem<string * string>> = [Paper "Magasine"; Glass "Bottle"; Glass "Jam"; Other ("Beer can", "Aluminium"); Other ("Bag", "Plastic")]
let items1 : list<TrashItem<string * string>> = [Paper "Magasine"; Paper "Bottle"; Paper "Jam";]

let fnPP(n,t) : string = 
    sprintf "%s (%s)" n t 

let ppTrashItem (fnPP:'a->string) (item:TrashItem<'a>) : string = 
    match item with 
    | Paper p -> sprintf "Paper (%s)"  p   
    | Glass i -> sprintf "Glass (%s)"  i 
    | Other o -> fnPP o   

ppTrashItem fnPP item1 
ppTrashItem fnPP item2
ppTrashItem fnPP item4

let isPaper (item:TrashItem<'a>) : bool = 
    match item with 
    | Paper x -> true 
    | Glass x -> false 
    | Other x -> false 

isPaper item2

type TrashCan<'a> = 
    Empty
    | TrashItems of TrashItem<'a> * TrashCan<'a> 

let addItem (item:TrashItem<'a>) (tc:TrashCan<'a>) : TrashCan<'a> = 
    TrashItems (item, tc)

addItem item1 Empty 

let rec ofList (ts:TrashItem<'a> list) : TrashCan<'a> = 
    match ts with 
    | [] -> Empty
    | x::rest -> 
            let trashI = x 
            let rest = ofList rest  
            TrashItems (trashI, rest)
ofList items 

let rec ofList1 (ts:TrashItem<'a> list) : TrashCan<'a> = 
    ts |> List.fold (fun s x -> addItem x s) Empty  

ofList1 items

let tcEx = ofList items
let tcEx1 = ofList items1

let rec forAll (fnP:TrashItem<'a>->bool) (tc:TrashCan<'a>) : bool = 
    match tc with  
    | Empty -> true 
    | TrashItems (item,can) -> if (fnP item) then forAll fnP can else false 

forAll isPaper tcEx


let isSameCategory (item1:TrashItem<'a>) (item2:TrashItem<'b>) : bool = 
    match (item1, item2 )with 
    | (Paper x, Paper y )-> true
    | (Glass x, Glass y )-> true
    | (Other x, Other y )-> true
    | (_,_) -> false 

isSameCategory item1 item3

let isSorted (tc:TrashCan<'a>) : bool = 
    match tc with 
    | Empty -> true 
    | TrashItems (item,can)-> 
                match item with 
                | Paper x -> if (forAll (isSameCategory item) can) then true else false 
                | Glass x -> if (forAll (isSameCategory item) can) then true else false 
                | Other x -> if (forAll (isSameCategory item) can) then true else false 

isSorted tcEx1

let rec fold (f:'a->TrashItem<'b>->'a) (e:'a) (tc:TrashCan<'b>) : 'a = 
    match tc with 
    | Empty -> e 
    | TrashItems (x, can) -> fold f (f e x) can 

fold (fun n _ -> n+1) 0 tcEx

let sort (tc:TrashCan<'a>) :  TrashCan<'a>*TrashCan<'a>*TrashCan<'a> = 
    let rec aux tc (paperTc, glassTc, otherTc) = 
        match tc with 
        | Empty -> (paperTc,  glassTc, otherTc)
        | TrashItems (item,can)-> 
                    match item with
                    | Paper x -> aux can ((addItem item paperTc), glassTc, otherTc)
                    | Glass x -> aux can (paperTc, (addItem item glassTc),  otherTc)
                    | Other x -> aux can (paperTc, glassTc, (addItem item otherTc))

    aux tc (Empty,Empty, Empty) 
    
let (paperTc, glassTc, otherTc) = sort tcEx

isSorted paperTc
isSorted glassTc
isSorted otherTc


let filter (fnP:TrashItem<'a> -> bool) (tc:TrashCan<'a>) : TrashCan<'a> =
    fold (fun acc item -> if fnP item then addItem item acc else acc) Empty tc

filter isPaper tcEx