// You MUST include explanations and comments to support your solutions.


type TrashItem<'a> =
    | Paper of string
    | Glass of string
    | Other of 'a

let item1 = Other("Shirt", "Clothes")
let item2 = Paper "Newspaper"
let item3 = Other("Sneakers", "Shoe")

let item4 = Glass "Wine glass"
let item5 = Glass "Ikea glass"

// What are the types of item1 and item2? Explain your answer
//      item1 = TrashItem<string * string>
//      item2 : TrashItem<string>
//      TrashItem is polymorfic, but Paper and Glass only accepts strings

// Is the type TrashItem monomorphic or polymorphic? Explain your answer.
//      Polymorphic

let (items: TrashItem<string * string> list) =
    [ Paper "magazine"
      Glass "Bottle"
      Glass "Jam"
      Other("Beer can", "Aluminium")
      Other("Bag", "Plastic") ]

let ppTrashItem (fnPP: 'a -> string) (item: TrashItem<'a>) : string =
    match item with
    | Paper itemName -> "Paper " + "(" + itemName + ")"
    | Glass itemName -> "Glass " + "(" + itemName + ")"
    | Other x -> fnPP x

let fnPP (n, t) = t + " (" + n + ")"

ppTrashItem fnPP item1 // val it : string = "Clothes (Shirt)"
ppTrashItem fnPP item2 // val it : string = "Paper (Newspaper)"
ppTrashItem fnPP item4 // val it: string = "Glass (Wine glass)"

let isPaper item =
    match item with
    | Paper s -> true
    | _ -> false

isPaper item1 // val it: bool = false
isPaper item2 // val it: bool = true

// 1.2.1
type TrashCan<'a> =
    | Empty
    | TrashItems of TrashItem<'a> * TrashCan<'a>

//  TrashItem<'a>-> TrashCan<'a>->TrashCan<'a>
let tc1 = Empty
let tc2 = TrashItems(item2, Empty)
let addItem (item: TrashItem<'a>) (tc: TrashCan<'a>) : TrashCan<'a> = TrashItems(item, tc)

TrashItems(Other("Shirt", "Clothes"), Empty)

let kaspersTrash = addItem item1 tc2
let kaspersTrash1 = addItem item2 kaspersTrash
let kaspersTrash2 = addItem item2 kaspersTrash1
let kaspersTrash3 = addItem item5 kaspersTrash2

// 1.2.2

// TrashItem<’a> list-> TrashCan<’a>
let items =
    [ Paper "magazine"
      Glass "Bottle"
      Glass "Jam"
      Other("Beer can", "Aluminium")
      Other("Bag", "Plastic") ]

let ofList (ts: TrashItem<'a> list) : TrashCan<'a> =
    List.fold (fun s e -> addItem e s) Empty (List.rev ts) // Empty is the accumulator, because it can hold trashitems.

let rec tcEx = ofList items

let forAll (fnP: TrashItem<'a> -> bool) (tc: TrashCan<'a>) : bool =
    let rec aux fnP tc1 =
        match tc1 with
        | Empty -> true // i know its true if its Empty because empty is the last item.
        | TrashItems(x, y) -> if (fnP x) then aux fnP y else false

    aux fnP tc

forAll isPaper Empty

// curried allows us to make partial application, if it was uncurried we could only give all inputs.
let rec isSameCategory (item1: TrashItem<'a>) (item2: TrashItem<'b>) : bool =
    match item1, item2 with
    | Paper _, Paper _ -> true
    | Glass _, Glass _ -> true
    | Other _, Other _ -> true
    | _, _ -> false

isSameCategory item1 item2
isSameCategory item1 item3
isSameCategory item4 item3

// 1.2.5
let isSorted (tc: TrashCan<'a>) : bool =
    match tc with
    | Empty -> true
    | TrashItems(x, y) -> forAll (isSameCategory x) y
// partial application use of isSameCategory
// fnP x becomes FnP x x where the predicate is same category

isSorted kaspersTrash2
// for all takes a predicate so we need to make a predicate
// is sorted checks them all.

// 1.3.1
// making our own fold ofc makes sense because its our own new type, not List, Map, Sequence but Trash
let rec fold (f: 'a -> TrashItem<'b> -> 'a) (e: 'a) (tc: TrashCan<'b>) : 'a =
    match tc with
    | Empty -> e
    | TrashItems(x, y) -> fold f (f e x) y // ( f e x ) becomes the number, we just keep passing f to next recursive iteration.

fold (fun n _ -> n + 1) 0 tcEx // Empty

// 1.3.2
let isGlass item =
    match item with
    | Glass s -> true
    | _ -> false

// 1.3.3
let sort (tc: TrashCan<'a>) : TrashCan<'a> * TrashCan<'a> * TrashCan<'a> =
    let paperTrash = Empty
    let glassTrash = Empty
    let otherTrash = Empty

    fold
        (fun (pTrash, gTrash, oTrash) e ->
            if isPaper e then
                (addItem e pTrash, glassTrash, otherTrash)
            elif isGlass e then
                (paperTrash, addItem e gTrash, otherTrash)
            else
                (paperTrash, glassTrash, addItem e oTrash))
        (paperTrash, glassTrash, otherTrash)
        tc
// husk alt inden for (fun ... oTrash)) er f i fold.
// Så kommer e som accumalator og tc.

let (paperTc, glassTc, otherTc) = sort tcEx

// 1.3.3
let filter (fnP: TrashItem<'a> -> bool) (tc: TrashCan<'a>) : TrashCan<'a> =
    fold (fun acc element -> if fnP element then addItem element acc else acc) Empty tc

filter isPaper tcEx // val it: TrashCan<string * string> = TrashItems (Paper "magazine", Empty)


// Question 2.
type Node<'a> =
    | Root of 'a
    | Link of 'a * Node<'a> ref

let mkRootElem a = ref (Root a)
let mkLinkElem a e = ref (Link(a, e))

let elemA = mkRootElem 'A'
let elemB = mkLinkElem 'B' elemA
let elemC = mkLinkElem 'C' elemB
let elemM = mkRootElem 'M'
let elemN = mkLinkElem 'N' elemM

let getVal (e: Node<'a> ref) : 'a =
    match !e with
    | Root e -> e
    | Link(a, e) -> a
// match e with
// | Root (a, e) -> e
// | Link (a, e) -> e

getVal elemC // let elemA = mkRootElem ’A’

let rec pathLength (e: Node<'a> ref) : int =
    let rec aux e acc =
        match !e with
        | Root(e) -> acc
        | Link(a, e1) -> aux e1 (acc + 1)

    aux e 0

List.map pathLength [ elemA; elemB; elemC ]
pathLength elemB


let find (e: Node<'a> ref) : Node<'a> ref =
    let rec aux (e: Node<'b> ref) =
        match e.Value with // ref defines on 
        | Root(_) -> e // we want to return e with is the node itself - not its value.
        | Link(a, e1) -> aux e1
    aux e

find elemM

find elemA = find elemB
find elemA = elemA
find elemB = elemA
find elemB = elemB
find elemC = find elemN

// References -> ref 
let union (e1: Node<'a> ref) (e2: Node<'a> ref) : Node<'a> ref =
    let elem1 = find e1
    let elem2 = find e2

    match elem2.Value with // root of N = M
    | Root v ->
        elem2.Value <- Link(v, elem1) // insert new value in e2'
        elem1    
    | Link(_, _) -> failwith "Not Implemented"
    //| Link _ -> failwith "union: got Link after find."

union elemA elemN
find elemN


// Question 3
let rec f x =
    function
    | [] -> []
    | y :: ys when x = y -> f x ys
    | y :: ys when x <> y ->
        match f x ys with
        | ys' -> y :: ys' // “sæt y foran resultat-listen”

let lst1 = f 2 [ 2; 2; 9; 9; 3 ]

// Create 3 examples that demonstrate what f computes.
let example1 = f 2 [ 2; 2; 9; 9; 3 ] // val example1: int list = [9; 9; 3]
let example2 = f "b" [ "a"; "a"; "b" ] // val example2: string list = ["a"; "a"]
let example3 = f 0.0 [ 2.0; 0.0; 9.0; 9.1; 3.3 ] // val example3: float list = [2.0; 9.0; 9.1; 3.3]

// Provide an appropriate name for f and explain why.
// reverseFilter f


// 3.2.1

// Thecompiler generates below warning when compiling function f.
// warning FS0025: Incomplete pattern matches on this expression.
// Explain why the warning is generated.
// ANSWER: The warning is indicating a case is not covered by the pattern(s). Example below: s
let missingPatternMatch item =
    match item with
    | x :: ys -> x

// 3.2.2.
// change the f function so it does not have compiler warning.

// ANSWER We remove "when" because it only works for one specific case, and replace it with
// if and else so we can cover all cases like bigger than < smaller than < and not only
// the two cases of = or not equal <>
let rec f2 x y =
    match y with
    | [] -> []
    | y :: ys ->
        if x = y then
            f2 x ys
        else
            match f2 x ys with // the result of this match is ys'
            | ys' -> y :: ys' // “sæt y foran resultat-listen returner den resultat listen

// Thefunction f is not tail-recursive. Explain why that is.
// It doesnt have an accumulator

// 3.3.2.
let fA x list =
    let rec aux x acc list =
        match list with
        | [] -> List.rev acc
        | y :: ys when x = y -> aux x acc ys
        | y :: ys when x <> y -> aux x (y :: acc) ys

    aux x [] list

let fa = fA 3 [ 3; 3; 4; 5 ]


// ------
let fSeq (x: 'a) (ys: seq<'a>) =
    seq {
        for n in ys do
            if x <> n then
                yield n
    }
// takes a polymorphic type returns a sequence

let nrs = fSeq 1 (seq { 1..10 })

// val nat : seq<int>   — nothing computed yet!

// Finite sequence from a list
let s =
    seq {
        yield 1
        yield 2
        yield 3
    }

Seq.item 4 nat

// Q 4

type Field = ValField of int
type FieldMap = Map<string, Field>

let fieldMap =
    Map.ofList [ ("A", ValField 42); ("B", ValField 43); ("C", ValField 0) ]

let fieldNames = List.map fst (Map.toList fieldMap)

let getField (fieldMap: Map<'a, 'b>) (fieldName: 'a) : 'b when 'a: comparison =
    if Map.containsKey fieldName fieldMap then
        Map.find fieldName fieldMap
    else
        failwith "union: got Link after find." // get value for key → 4

List.map (getField fieldMap) fieldNames
// evaluates to [ValField 42;ValField 43;ValField 0].

let testField = ValField 5

let getFieldValue (field: Field) : int =
    match field with
    | ValField v -> v

getFieldValue testField

let lookupFieldValue (fieldMap: Map<'a, Field>) (fieldName: 'a) : int when 'a: comparison =
    (getField fieldMap >> getFieldValue) fieldName // look up function composition
// let field  = getField fieldMap fieldName
// getFieldValue field
// OR: getFieldValue getField fieldMap fieldName

List.map (lookupFieldValue fieldMap) fieldNames //  evaluates to [42;43;0]

let updateField (fieldMap: Map<string, Field>) (fieldName: string) (newValue: int) : Map<string, Field> =
    if Map.containsKey fieldName fieldMap then
        Map.add fieldName (ValField newValue) fieldMap
    else
        raise (System.Exception("Can't pop empty stack"))

updateField fieldMap "A" 32

// evaluatestomap[("A",ValField32);("B",ValField 43);("C",ValField 0)]

// 4.2
type Function = FnOneToOne of string * string * (int -> int)
type FuncMap = Map<string, Function>
let fnAddTwoAC = ("fnAddTwoAC", FnOneToOne("A", "C", fun a -> a + 2))
let fnNegateAB = ("fnNegateAB", FnOneToOne("A", "B", fun a -> -a))
let fnNegateDB = ("fnNegateDB", FnOneToOne("D", "B", fun a -> -a))
let funcMap = Map.ofList [ fnAddTwoAC; fnNegateAB; fnNegateDB ]


let evalFn (fieldMap: Map<string, Field>) (fn: Function) : Map<string, Field> =
    match fn with
    | FnOneToOne(first, second, fInput) ->
        let getVal = lookupFieldValue fieldMap first
        let newVal = fInput getVal
        updateField fieldMap second newVal

evalFn fieldMap (snd fnAddTwoAC)
// take the first key value in the map, do the function then put it on c

let findFunctionsBySource (sourceField: string) (funcMap: Map<'a, Function>) : Function list when 'a: comparison =
    List.map snd (List.filter (fun (_, FnOneToOne(x, _, _)) -> sourceField = x) (Map.toList funcMap)) // |> List.map snd
// take all 2nd values. filter on A
findFunctionsBySource "A" funcMap

let findFunctionsBySource_emanuel
    (sourceField: string)
    (funcMap: Map<'a, Function>)
    : Function list when 'a: comparison =
    funcMap
    |> Map.toList
    |> List.filter (fun (_, FnOneToOne(a, _, _)) -> a = sourceField)
    |> List.map snd

findFunctionsBySource_emanuel "A" funcMap

let evalField (fieldName: string) (funcMap: Map<'a, Function>) (fieldMap: Map<string, Field>) = // : Map<string,Field>  'a : comparison =
    List.fold evalFn fieldMap (findFunctionsBySource fieldName funcMap)

evalField "A" funcMap fieldMap
