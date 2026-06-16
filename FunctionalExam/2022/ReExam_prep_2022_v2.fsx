
type TrashItem<'a> = 
    Paper of string 
    | Glass of string 
    | Other of 'a 

let item1 = Other ("Shirt", "Clothes") 
let item2 = Paper "Newspaper"
let item3 = Other ("Sneakers", "shoes") 
let item4 = Glass "Wine glass" 

// item1: TrashItem<String * string>
// item2: TrashItem<'a'>
// TrashItem polymorphic because of the <'a> 

let items : TrashItem<string*string> list = [Paper "Magazine"; Glass "Bottle"; Glass "Jam"; Other ("Beer can", "Aluminium"); Other ("Bag", "Plastic")]

let fnPP (n,t) = 
    sprintf "%s (%s)" n t 

let ppTrashItem (fnPP:'a->string) (item:TrashItem<'a>) : string = 
    match item with 
    | Paper x -> sprintf "Paper (%s)" x
    | Glass x -> sprintf "Glass (%s)" x 
    | Other x -> fnPP x 

ppTrashItem fnPP item1;
ppTrashItem fnPP item4;;
ppTrashItem fnPP item2;;


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

let ofList (ts:TrashItem<'a>list) : TrashCan<'a> = 
    ts |> List.fold (fun acc x -> addItem x acc) Empty 

let TcEx = ofList items


let rec forAll (fnP:TrashItem<'a>->bool) (tc:TrashCan<'a>) : bool = 
    match tc with 
    | Empty -> true 
    | TrashItems (item, can) -> 
                        if (fnP item) then forAll fnP can else false  

forAll isPaper Empty
forAll isPaper TcEx


let isSameCategory (item1:TrashItem<'a>) (item2:TrashItem<'a>) : bool = 
    match (item1, item2) with
    | (Paper _, Paper _ ) ->  true 
    | (Glass _, Glass _ ) ->  true 
    | (Other _, Other _ ) ->  true 
    | (_,_) -> false 

isSameCategory item1 item3

let isSorted (tc:TrashCan<'a>) : bool = 
    match tc with 
    | Empty -> true 
    | TrashItems (item, can) ->  
               forAll (isSameCategory item ) can 

isSorted TcEx
isSorted Empty

let rec fold (f: 'a->TrashItem<'b>->'a) (e:'a) (tc:TrashCan<'b>) : 'a = 
    match tc with 
    | Empty -> e 
    | TrashItems (item, can) -> 
               fold f (f e item ) can  

fold (fun n _ -> n+1) 0 TcEx

let sort (tc:TrashCan<'a>) : TrashCan<'a> * TrashCan<'a> * TrashCan<'a> = 
    fold (fun (pTc, gTc, oTc) item -> 
                     match item with 
                     | Paper x -> (addItem item pTc, gTc, oTc )
                     | Glass x -> (pTc, addItem item gTc, oTc )
                     | Other x -> (pTc, gTc, addItem item oTc )
    
    ) (Empty, Empty, Empty) tc 

let (paperTc, glassTc, otherTc) = sort TcEx

let filter (fnP:TrashItem<'a>->bool) (tc:TrashCan<'a>) : TrashCan<'a> = 
    fold (fun acc item -> if (fnP item) then addItem item acc else acc) Empty tc   

filter isPaper TcEx 