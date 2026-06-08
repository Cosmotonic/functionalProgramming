
type stack<'b> = list<'b>
type mapStack<'a,'b> = MapStack of list<'a*stack<'b>>

let ex1 = MapStack [('1',[3;1;1]); ('2',[1;3;1]); ('3',[1;1;3]);('4',[3;3;1]); ('5',[2;3;4]); ('6',[2;1;2])]

// 1.1.1 
let ex2 = MapStack [("Hey", ['H';'E';'Y']);("there", ['t';'h';'e';'r';'e'])]; 

// 1.1.2 
// val ex2: mapStack<string,char>

// 1.1.3 
let empty() : mapStack<'a,'b> = MapStack[]

// 1.1.4
let rec numKeys (m:mapStack<'a, 'b>) : int = 
    match m with 
    | MapStack[] -> 0 
    | MapStack (h::t) -> 1 + numKeys (MapStack t)   

numKeys ex1 

// 1.1.5 
let rec numValues (m:mapStack<'a,'b>) : int = 
    match m with 
    | MapStack [] -> 0 
    | MapStack (x::rest) -> 
                    let k, v  = x
                    (List.length v) + numValues (MapStack rest)
numValues ex1 

// 1.2.1 
let pop (s:'a list) : 'a * 'a list = 
    match s with 
    | [] -> raise (System.Exception "Cant pop empty stack" )
    | x::y -> (x, y)
pop [1;2;3]

let remove (k:'a) (l:('a * 'b) list ) : ('a * 'b) list = 
    l |> List.filter (fun (x,y) -> x <> k )

remove 1 [(1, "v1"); (2, "v2"); (3, "v3"); (4, "v4") ] 

let rec mapPush (k:'a) (v:'b) (m:mapStack<'a, 'b>) : mapStack<'a, 'b> when 'a : equality = 
    match m with 
    | MapStack [] ->  MapStack [(k, [v])]
    | MapStack ((key, value)::rest)  -> 

    
    
