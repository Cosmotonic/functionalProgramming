
// Question 1. 

type Album = Elem list 
and Elem = 
    PicElem of string 
    | AlbumElem of string * Album 

let pic1 = PicElem "Peter.jpg"
let pic2 = PicElem "Gitte.jpg" 
let pic3 = PicElem "Mother.jpg" 
let pic4 = PicElem "Father.jpg" 
let pic5 = PicElem "Hans.jpg" 

let albumElem1 = AlbumElem ("Family", [pic3; pic4])
let albumElem2 = AlbumElem ("School", [pic5])
let albumelem3 = AlbumElem ("All", [pic1; albumElem1; pic2; albumElem2])
let album = [albumelem3]


// Question 3.  
type MultiStack<'a, 'b> = {listA:('a*int) list;
                           listB:('b*int) list}

let exMS01 = {listA = [("Copenhagen", 2); ("Aarhus", 0)];
              listB = [(4000,1)]}

let exMS02 = {listA = [(100,2); (42, 2)];
              listB = [('Q',3);('C',0)]}


let mkMultiStack :  MultiStack<'a,'b> =  {listA=[]; listB=[]}


let size (ms:MultiStack<'a,'b>) : int = 
    let sizeA = ms.listA |> List.length 
    let sizeB = ms.listB |> List.length 
    sizeA + sizeB

size exMS01

let isEmpty (ms:MultiStack<'a, 'b>) : bool = 
    if (size ms) > 0 then false 
    else true 

isEmpty mkMultiStack
let exMS03 : MultiStack<int,int> = { listA = []; listB = []} 

let pushA (e:'a) (ms:MultiStack<'a,'b>) : MultiStack<'a,'b> = 
    let listA = ms.listA  
    let index = size ms 
    let newTouple = [(e,index)]
    let joined = newTouple @ listA
    {listA = joined; listB=ms.listB}

pushA "Faxe" exMS01
pushA 42 exMS03

let pushB (e:'b) (ms:MultiStack<'a, 'b>) : MultiStack<'a,'b> = 
    {listA=ms.listA; listB = [(e,(size ms))] @ (ms.listB); }
pushB 4640 exMS01


let pop (ms:MultiStack<'a,'b>) : ('a option * 'b option) * MultiStack<'a, 'b> = 
    // get highst index 
    let valA, indA = List.head   ms.listA  
    let valB, indB = List.head   ms.listB 
    if indA > indB then 
                    let newList =  ms.listA |> List.removeAt 0
                    ((Some valA, None), {listA = newList; listB=ms.listB})
    else 
        let newList = ms.listB |> List.removeAt 0    
        ((None, Some valB), {listA = ms.listA; listB=newList})

pop exMS01

let rec checkList (lst:'a list ) : bool = 
    match lst with 
    | [] -> true 
    | [x] -> true 
    | (x1,x2)::(y1,y2)::rest -> if x2 > y2 then checkList( (y1,y2)::rest ) else false 

let chkDecreasing (ms:MultiStack<'a, 'b> ) : bool = 
    if (checkList ms.listA) && (checkList ms.listB) then true else false  
chkDecreasing exMS01 

let forAll (fnA:'a->bool) (fnB:'b->bool) (ms:MultiStack<'a, 'b>) : bool = 
    // let testA = fnA (ms.listA) 
    let testA = ms.listA  |> List.forall (fun (x,indx) -> (fnA x))
    let testB = ms.listB  |> List.forall (fun (y,indy) -> (fnB y)) 
    if testA && testB then true else false 

forAll (fun x -> String.length x> 2) ((<) 1000) exMS01


let map (fnA:'a->'b) (fnB:'c->'d) (ms:MultiStack<'a,'c>) : MultiStack<'b,'d> = 
    let mapA = ms.listA |> List.map (fun (x,y) -> ((fnA x),y)) 
    let mapB = ms.listB |> List.map (fun (x,y) -> ((fnB x),y)) 
    {listA = mapA; listB= mapB}

map String.length (fun x -> x.ToString()) exMS01


