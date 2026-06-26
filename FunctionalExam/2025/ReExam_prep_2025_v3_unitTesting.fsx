
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


// 2.
let log s = printfn "--> %s" s
let clean() = log "cleaning..."
let build() = log "building..."
let unitTest() = log "unit testing..."
let releaseTest() = log "release testing..."
let deploy() = log "deploying..."

let cpFile (src:string) (dst:string) = 
     log (sprintf "---> copying file from %s to %s" src dst ) 
   
cpFile "foo.txt" "bar/"; 
let copyFoo = cpFile "foo.txt" "bar/"

let chain (a1 : unit -> unit) (a2 : unit -> unit) : unit -> unit =
    fun () ->
        a1 ()
        a2 ()
let allBuild = chain build deploy 

let (++) a1 a2 =
    fun () ->
        a1 ()
        a2 ()

let allTest = unitTest ++ releaseTest
let tests = unitTest ++ releaseTest
let releaseBuild =
    clean ++ build ++ tests ++ deploy
let pipeline =
    clean ++ build ++ unitTest ++ deploy

let extraChecks =
    if 0>1 then // isFriday 
        allTest // We dont want to release on fridays
    else
        pipeline 

// 2.2 
type buildStep = {
    name: string;
    action: unit -> unit;
    deps: string list
    }

let mkBuildStep (name:string) (action:(unit->unit)) : buildStep = 
    {
        name = name
        action = action
        deps = []
    }

let cleanBS = mkBuildStep "clean" clean;
let buildBS = mkBuildStep "build" build;

let dependsOn name bs : buildStep =
    { bs with deps = name :: bs.deps }

let buildBS' = dependsOn "clean" buildBS

let testBS =
    mkBuildStep "test" allTest
    |> dependsOn "build"
    |> dependsOn "clean"

// 2.3 
type buildStepEnv = Map<string,buildStep>
let bsEnv = Map.ofList [ ("clean", cleanBS);
                                    ("build",buildBS);
                                    ("test", testBS)]

let lookupBS name (env: Map<string,'a>) : 'a =
    match Map.tryFind name env with
    | Some bs -> bs
    | None -> raise (System.Exception("Failed to find build step"))

lookupBS "clean" bsEnv


let rec getDeps (name:string) (env:Map<string,buildStep>) : string list =
    let bs = lookupBS name env

    let deps =
        bs.deps
        |> List.collect (fun dep -> getDeps dep env)

    List.distinct (deps @ [name])

getDeps "clean" bsEnv
// ["clean"]

getDeps "test" bsEnv
// ["clean"; "build"; "test"]


let exec name (env: Map<string, buildStep>) : unit =
    let rec run visited name =
        if List.contains name visited then
            visited
        else
            let bs = lookupBS name env
            let visitedAfterDeps =
                List.fold run visited bs.deps
            bs.action ()
            name :: visitedAfterDeps
    run [] name |> ignore

exec "clean" bsEnv

// 3. 

let catMapSlow n =
    let cache = Map.empty
    if n < 0 then failwith "Error, negative argument"
    else
        let rec cat n =
            if n = 0 then 1
            else
                if Map.containsKey n cache then cache[n]
                else
                    let sum =
                        List.fold (fun s i -> s + cat i * cat (n-1-i))
                                  0
                                  [0..n-1]
                    Map.add n sum cache   // <-- compiler warning
                    sum
        cat n

let catMapFast n =
    let rec cat n cache =
        if n = 0 then
            (1, cache)
            // Returnerer både resultat og cache.
        else
            match Map.tryFind n cache with
            | Some value ->
                (value, cache) // Vi fandt værdien i cachen. // Ingen rekursion nødvendig.
            | None ->
                let mutable cache' = cache
                let sum =
                    [0 .. n-1]
                    |> List.sumBy (fun i ->

                        let (left, c1) = cat i cache'
                        cache' <- c1  // Gem eventuelle nye cache-værdier.
                        let (right, c2) = cat (n-1-i) cache'
                        cache' <- c2 // Gem endnu flere cache-værdier.
                        left * right)
                let cache'' =
                    Map.add n sum cache'
                    // HER gemmes resultatet faktisk.
                (sum, cache'')
                // Returnerer både værdi og opdateret cache.
    fst (cat n Map.empty)

let mutable value = 10 
// 4 
let rec countIf p = function
    [] -> 0
    | lst -> 
            let (x::xs) = lst
            let count = countIf p xs
            if p x then count + 1 else count

countIf ((>)0)[-1;2;-4;5]


let rec countIfA p lst acc =
    match lst with 
    | [] -> acc
    | x::xs -> 
            if p x then countIfA p xs (acc+1)
            else  
            countIfA p xs acc

let lst = [-1;2;-4;5]
countIfA ((>) 0) lst 0

// 

let maxList (xs:'a list) : 'a when 'a : comparison = 
    let rec aux xs s = 
        match xs with 
        | [] -> s 
        | h::t -> if h > s then aux t h 
                    else aux t s  
    aux xs xs.Head

maxList ['a';'b';'d']

