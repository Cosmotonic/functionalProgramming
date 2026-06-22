
type stack<'b> = list<'b>
type mapStack<'a,'b> = MapStack of list<'a*stack<'b>>

let ex1 = MapStack [('1',[3;1;1]); ('2',[1;3;1]); ('3',[1;1;3]);('4',[3;3;1]); ('5',[2;3;4]); ('6',[2;1;2])]
let ex4 = MapStack [('1',[]); ('2',[1;3;1]); ('3',[1;1;3]);('4',[3;3;1]); ('5',[2;3;4]); ('6',[2;1;2])]

let ex2 = MapStack [("Hey", ['H';'e';'y']); ("there", ['t';'h';'e';'r';'e'])]
let ex3 = MapStack [("Hey", ['H';'e';'y']); ("1", [])]

// MapStack<String,Char>

let empty() : mapStack<'a,'b> = MapStack[]; 

let numKeys (m:mapStack<'a,'b>) : int = 
    let (MapStack list) = m 
    list |> List.length 

numKeys ex1 

let rec numValues (MapStack m:mapStack<'a,'b>) : int = 
    match m with 
    | [] -> 0 
    | (_,stack)::y -> 
                let lstLen = stack |> List.length
                let newM = numValues (MapStack y) 
                lstLen + newM
numValues ex1

// 1.2 
let pop (s:'a list) : 'a * 'a list = 
    match s with 
    | [] -> raise (System.Exception "Cant pop exmpty stack") 
    | x::tail -> (x,tail) 
pop [1..3]

let remove (k:'a) (l:('a * 'b) list) : ('a * 'b) list when 'a : equality = 
    l |> List.filter (fun (key,_) ->  key <> k )
    
remove 1 [(1,"v1");(2,"v2");(3,"v3")]

// you need equality because we are comparing two elements, 
// if they are to be matched they have to be matchable pairs. 

let rec mapPush (k:'a) (v:'b) (MapStack m) : mapStack<'a,'b> when 'a : equality = 
    match m with 
    | [] -> MapStack [(k,[v])]
    | (key,value)::rest -> 
            if k <> key then 
                let current = (key, value) 
                let (MapStack CheckNext) = mapPush k v (MapStack rest)
                MapStack ((key, value)::CheckNext)
            else 
                MapStack((key,v::value)::rest)

mapPush '2' 5 ex1
mapPush '8' 1 ex1

let rec mapPop (k:'a) (MapStack m) : 'b * mapStack<'a, 'b> when 'a: equality = 
    match m with 
    | [] -> raise (System.Exception "Can't find key.")
    | (key,v::lst)::rest ->
                if k = key && (List.length (v::lst) > 0) then 
                    (v, MapStack ((key,lst)::rest))
                else 
                    let currentK, currentV = (key, v::lst) 
                    let rtnV, (MapStack last) = mapPop k (MapStack rest)
                    (rtnV, MapStack ((currentK,currentV)::last))
    | _ -> raise (System.Exception "Stack is empty")

mapPop '3' ex1
            
let map (f:'a->'b->'c) (MapStack m: mapStack<'a,'b>) : mapStack<'a,'c> = 
    let newM = m |> List.map (fun (key, values) -> key, (values |> List.map (fun v -> f key v))) 
    MapStack newM 

map (fun _ v -> v+1) ex1 



let folder (l: 'b list) (e:'a) : 'a = 
    l |> List.fold (fun acc x -> acc + x.ToString()) e  
folder ['a';'b';'c'] ""


let rec fold (f:'a->'b->'c->'a) (e:'a) (MapStack m : mapStack<'b,'c>) : 'a = 
    match m with 
    | [] -> e 
    | (key, stack )::tail -> 
            let combined = stack |> List.fold (fun acc v -> f acc key v) e 
            fold f combined (MapStack tail) 
    
fold (fun str _ v -> str + v.ToString()) "" ex2


