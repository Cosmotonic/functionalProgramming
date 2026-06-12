
// Question 1. 
type stack<'b> = list<'b>
type mapStack<'a,'b> = 
    MapStack of list<'a*stack<'b>>

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

let rec mapPush (k:'a) (v:'b) (MapStack m:mapStack<'a, 'b>) : mapStack<'a, 'b> when 'a : equality = 
    match m with
    | [] ->  MapStack [(k, [v])]
    | (key, values)::rest ->
        if key = k then
            MapStack ((key, v::values)::rest)
        else
            let (MapStack newRest) = mapPush k v (MapStack rest)
            MapStack ((key, values)::newRest)

let rec mapPop (k:'a)(MapStack m) : 'b * mapStack<'a, 'b> when 'a : equality = 
    match m with 
    | [] -> raise (System.Exception "Cant find key")
    | (key, values)::rest ->
                    if k = key then 
                        match values with 
                        | [] -> raise (System.Exception "cant pop empty stack")
                        | h::t -> (h, MapStack ((key, t)::rest))
                    else 
                        let (h, MapStack newRest) = mapPop k ( MapStack rest) 
                        (h, MapStack ((key, values)::newRest))
mapPop '1' ex1

// 1.3.1 
let rec map (f:'a->'b->'c) (MapStack m):mapStack<'a,'c>=
    match m with
    |[] -> MapStack []
    |(k,stack)::tail-> 
        let newstack= List.map(fun v -> f k v) stack
        let (MapStack rest) = map f (MapStack tail)
        MapStack ((k,newstack)::rest)
map (fun _ v-> v+1) ex1

// 1.3.1 
let rec map2 f (MapStack m)  = 
    match m with 
    | [] -> MapStack [] 
    | (k,stack)::tail -> 
                let newStack = stack |> List.map (fun x -> f k x) // |> List.map (fun x -> f k x) // 
                let (MapStack rest) = map2 f (MapStack tail) 
                MapStack ((k,newStack)::rest)

map2 (fun _ v-> v+1) ex1


let rec map3 f (MapStack m)  = 
    match m with 
    | [] -> MapStack []
    | (k,v)::tail -> 
                let updatedStack = v |> List.map ( fun x -> f k x)
                let (MapStack rest) = map3 f (MapStack tail) 
                (MapStack ((k, updatedStack)::rest))


let rec fold f e (MapStack m) : 'a = 
    match m with 
    | [] -> e
    | (k,stack)::tail -> 
            let word = stack |> List.fold (fun s v -> f s k v ) e
            // let word = stack |> List.map (fun x -> f x) |> List.fold (fun s x ->  x + s) e 
            fold f word (MapStack tail)

fold (fun str _ v -> str + v.ToString()) "" ex2 


let hey = ['H';'E';'Y']

let folder lst = 
    lst |> List.fold (fun s x -> s+string(x) ) ""

folder hey 


