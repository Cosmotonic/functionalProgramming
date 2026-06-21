
type stack<'b> = list<'b>
type mapStack<'a,'b> = MapStack of list<'a*stack<'b>>


let ex1 = MapStack [('1',[3;1;1]); ('2',[1;3;1]); ('3',[1;1;3]);('4',[3;3;1]); ('5',[2;3;4]); ('6',[2;1;2])]

let ex2 = MapStack [("Hey", ['H';'e';'y']); ("Hey", ['t';'h';'e';'r';'e'])]

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

