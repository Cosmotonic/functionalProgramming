// I hereby declare that I myself have created this exam hand–in in its entirety without help from AI tooling and anybody else.
(*Exam 2025*)
// --------------------------------------------------------------------------------------------------
// -----------------------------------Assignment 1---------------------------------------------------
// --------------------------------------------------------------------------------------------------

type stack<'b> = list<'b>
type mapStack<'a,'b> = 
    MapStack of list<'a*stack<'b>>
let ex1 = 
    MapStack [('1',[3;1;1]); ('2',[1;3;1]); ('3',[1;1;3]);
        ('4',[3;3;1]); ('5',[2;3;4]); ('6',[2;1;2])]

//---------------------------------------------------------1.1----------------------------------------------------------
//1.11
let ex2 = 
    MapStack [("Hey",['H';'e';'y']); ("there",['t';'h';'e';'r';'e']); ("",[])]
//1.12
//The type of the value ex2 is mapStack<string,char>, as its a mapStack that takes strings as the key and maps to the values.
//the values in each stack are of type character. (the stack and the Map are polymorphic)

//1.13
let empty () = MapStack[]
//1.14
let numKeys (m:mapStack<'a,'b>):int =
    match m with
    |MapStack list-> List.length list 
numKeys ex1
numKeys ex2

let numKeys2 (MapStack m ) = List.length m
//1.15

let numValues  (MapStack m ):int =  
        List.fold(fun acc (v,s)  -> List.length s + acc) 0 m

numValues ex1    

let numValue2 (m:mapStack<'a,'b>):int =
    match m with
    |MapStack list -> list |> List.sumBy (fun (_,s)-> List.length s)
numValue2 ex2
//---------------------------------------------------------1.2----------------------------------------------------------
//1.21

let pop (s:'a list):'a * 'a list = 
    match s with
    |[] -> raise (System.Exception "Can't pop empty stack") 
    |(sh::st) -> (sh , st)

pop [1;2;3]
//1.22

let rec remove (k:'a) (l:('a*'b) list):('a*'b) list = 
    match l with
    |[]->[]
    |(k2,v2)::tail-> 
        if k = k2 then tail 
        else (k2,v2)::remove k tail

remove 1 [(1,"v1");(2,"v2");(3,"v3")]
let remove2 k l=
    List.filter (fun (k2,_) -> k <> k2) l
remove2 1 [(1,"v1");(2,"v2");(3,"v3")]
// It means that the type 'a must support equality comparison, so values of type 'a can be compared using =
// because the compiler can't assume all types support = by default (e.g. function types can't be compared). The constraint makes it explicit.
//So for example in the fucntion remove, we need to compare  if k = k2 then tail and the function needs to support that.

// The constraint when 'a: equality means that the key type 'a must admit equality comparison. 
// It is necessary because remove compares keys using k = k2 in order to decide whether a pair should be removed.Because we are comparing the keys of type 'a
//1.23
let removeMapStack (MapStack ms) = ms
let mapPush (k:'a) (v:'b) (MapStack m):mapStack<'a,'b> =
    let rec aux k v m =
        match m with
        |[] -> MapStack [k,[v]]
        |(k1,k2)::tail->
        if k = k1 then MapStack ((k1,v::k2)::tail)
        else 
            let (MapStack result) = aux k v tail
            MapStack ((k1,k2)::result)
    aux k v m
mapPush '0' 2 ex1


let mapPush2 k v (m:mapStack<'a,'b>):mapStack<'a,'b> = 
    match m with 
    |MapStack list-> //match with type
        let rec push lst = //then seperate to compare
            match lst with
            |[]->[(k,[v])]
            |(k2,s)::tail->
                if k = k2 then (k2, v::s)::tail
                else (k2,s)::push tail
        MapStack (push list)

mapPush2 '0' 2 ex1

//1.24

let mapPop (k:'a) (MapStack m):'b * mapStack<'a,'b> =
    let rec aux k m=
        match m with
        | [] -> raise (System.Exception "Can't find key")
        | (k1, [])::tail -> raise (System.Exception "Can't pop empty stack")
        | (k1, v1::rest)::tail ->
            if k1 = k then 
                (v1, MapStack ((k1,rest)::tail))
            else
                let (v,MapStack result)= aux k tail
                (v, MapStack ((k1,v1::rest)::result))

    aux k m    
mapPop '3' ex1
let test = 
    MapStack [('1',[]); ('2',[1;3;1]); ('3',[1;1;3]);
        ('4',[3;3;1]); ('5',[2;3;4]); ('6',[2;1;2])]
mapPop '1' test
let mapPop2 k (m:mapStack<'a,'b>):'b*mapStack<'a,'b> = 
    match m with
    |MapStack list ->    // unwrap the MapStack to get the raw inner list
        let rec pop1 lst = // recursive helper that works on the raw list
            match lst with
            |[]-> raise (System.Exception "Can't find key.")  // key was never found, throw exception
            |(k2,s)::tail->                             // destructure: k2 is current key, s is its stack, tail is the rest
                if k = k2 then                          // found the key we're looking for
                    let (v, remaining) = pop s  // pop the top of the stack: v is the value, remaining is the rest of the stack
                    (v, (k2, remaining) :: tail)        // return v + rebuilt list with updated (shorter) stack for k2
                else 
                    let (v, updatedList) = pop1 tail// key not here, recurse on tail to keep searching
                    (v, (k2, s) :: updatedList)         // on the way back up, prepend the current (untouched) pair back
        let (v, updatedList) = pop1 list  // call pop1 on the full inner list, get back popped value + modified list
        (v, MapStack updatedList)                       // re-wrap the modified list in MapStack and return alongside the popped value       
mapPop2 '3' ex1
mapPop2 '0' ex1 //tests error handling
// Test 3: key exists but stack is empty -> "Can't pop empty stack"
let testEmpty = MapStack [('x', [])]
mapPop 'x' testEmpty

//---------------------------------------------------------1.3----------------------------------------------------------
//1.31
let rec map (f:'a->'b->'c) (MapStack m):mapStack<'a,'c>=
    match m with
    |[] -> MapStack []
    |(k,stack)::tail-> 
        let newstack= List.map(fun v-> f k v) stack
        let (MapStack rest) = map f (MapStack tail)
        MapStack ((k,newstack)::rest)

map (fun _ v -> v + 1) ex1

//1.32
let fold (f:'a->'b->'c->'a) (e:'a) (m:mapStack<'b,'c>):'a = 
    match m with
    |MapStack []-> e
    |MapStack list->    List.fold(fun acc (k,stack)  -> 
                                                    List.fold(fun acc2 v-> f acc2 k v) acc stack) e list

fold (fun str _ v -> str + v.ToString()) "" ex2

// --------------------------------------------------------------------------------------------------
// -----------------------------------Assignment 2---------------------------------------------------
// --------------------------------------------------------------------------------------------------

//---------------------------------------------------------2.1----------------------------------------------------------
//2.11
let log s = printfn "--> %s" s
let clean() = log "cleaning..."
let build() = log "building..."
let unitTest() = log "unit testing..."
let releaseTest() = log "release testing..."
let deploy() = log "deploying..."

let cpFile src dst () =  log $"copying file from {src} to {dst} ..."

cpFile "foo.txt" "Bar/" ()
cpFile "foo.txt" "Bar/" 
// -------By supplying only the first two arguments (src and dst), cpFile returns a function waiting for unit. 
// -------That returned function is the action, which can later be executed in a build pipeline.

// cpFile "foo.txt" "Bar/" → returns a unit -> unit function, nothing happens yet
// cpFile "foo.txt" "Bar/" () → gives it the final (), now it prints!

// The () is essentially the trigger that says "okay, execute now". 
// Until you provide it, you just have a function sitting there waiting. This is what makes it an action — it's a function waiting for () to run!

// //So myCopyAction is an action that when called with ():
// In this exam → just prints "--> copying file from foo.txt to Bar/ ..."
// In a real program → would actually copy the file
//2.12
let chain (a1:unit->unit) (a2:unit->'a) () = 
    a1()
    a2()

let allTest1 = chain unitTest releaseTest
allTest1 ()
//2.13
let (++) = chain 
let allTest2= unitTest ++ releaseTest
allTest2 ()
//---------------------------------------------------------2.2----------------------------------------------------------
type buildStep = {
    name: string;
    action: unit -> unit;
    deps: string list
}
//2.21
let mkBuildStep (name:string) (action: unit->unit):buildStep = { name = name; action = action; deps = [] }
let cleanBS = mkBuildStep "clean" clean
let buildBS = mkBuildStep "build" build

//2.22
let dependsOn (name:string) (bs:buildStep):buildStep = {bs with deps = bs.deps@[name]}
let dependsOn1 (name:string) (bs:buildStep):buildStep = {bs with deps = name::bs.deps}
let dependsOn2 (name:string) {name=name; action=action;deps=deps} = {name=name; action=action;deps=name::deps}

let buildBS' = dependsOn "clean" buildBS

//2.23
let testBS = dependsOn "build" (dependsOn "clean"(mkBuildStep "test" allTest2))

//---------------------------------------------------------2.3----------------------------------------------------------
//2.31
type buildStepEnv = Map<string,buildStep>
let bsEnv = Map.ofList [("clean",cleanBS);
    ("build",buildBS');
    ("test", testBS)]

let lookupBS (name:string) (env:Map<string,'a>):'a = 
    if Map.containsKey name env then Map.find name env
    else raise (System.Exception "Failed to find build step")

lookupBS "clean" bsEnv
//another version(faster)
let lookupBS2 (name:string) (env:Map<string,'a>):'a =
    match Map.tryFind name env with
    | Some bs -> bs
    | None -> raise (System.Exception "Failed to find build step")
lookupBS2 "clean" bsEnv

//2.32
let rec getDeps name env =
    let bs=lookupBS name env
    let depsList = List.collect (fun dep -> getDeps dep env) bs.deps
    let uniqueDeps = List.distinct depsList
    if List.contains name uniqueDeps then uniqueDeps
    else uniqueDeps@[name]
getDeps "test" bsEnv

//2.33
//hardest he said
let exec name env =
    let deps= getDeps name env
    let rec aux deps =
        match deps with
        |[]->()
        |h::tail ->  
            let  bs= lookupBS h env
            bs.action ()
            aux tail
    aux deps
exec "test" bsEnv                   
// --------------------------------------------------------------------------------------------------
// -----------------------------------Assignment 3---------------------------------------------------
// --------------------------------------------------------------------------------------------------

//---------------------------------------------------------3.1----------------------------------------------------------
//3.11
let triangular n= 
    if n<0 then raise (System.Exception "Error, negative argument")
    else (n*(n+1))/2
triangular 3
//3.12
let triangularSeq= Seq.initInfinite (fun n -> triangular(n+1))

Seq.take 4 triangularSeq
//3.13
let triangularSeq'= Seq.cache(Seq.initInfinite (fun n -> triangular(n+1)))
//---------------------------------------------------------3.2----------------------------------------------------------
//3.21
let catMapSlow n =
    if n < 0 then failwith "Error, negative argument"
    else
        let cache = Array.create (n+1) 0
        let rec cat n =
            if n = 0 then 1
            else
                if cache[n] <> 0 then cache[n]
                else
                    let sum = List.fold (fun s i -> s + cat i * cat (n-1-i)) 0 [0..n-1]
                    cache[n] <- sum
                    sum       
        cat n
catMapSlow 17
//catMapSlow is slow because the cache is never actually updated. The line Map.add n sum cache creates a new map with the cached value, but the result is immediately discarded — the original cache variable never changes because F# maps are immutable.
// This means every call to cat n always finds an empty cache, so it always recomputes everything from scratch. This leads to exponential recomputation — for example cat 5 calls cat 4, cat 3, cat 2 etc., and each of those calls the same functions again and again.
// The compiler even warns about this with:

// "The result of this expression has type 'Map<int,int>' and is implicitly ignored"

// This is the exact bug — the updated map is returned but never used.

// The key insight is just two things:

// Maps are immutable — Map.add returns a new map, it doesn't modify the old one
// The new map is thrown away — so the cache is always empty

//3.23
let catArray n =
    if n < 0 then failwith "Error, negative argument"
    else
        let cache = Array.create (n+1) -1
        let rec cat n =
            if n = 0 then 1
            else
                if cache[n] <> -1 then cache[n]
                else
                    let sum = List.fold (fun s i -> s + cat i * cat (n-1-i)) 0 [0..n-1]
                    cache[n] <- sum
                    sum
        cat n
catArray 17

// --------------------------------------------------------------------------------------------------
// -----------------------------------Assignment 4---------------------------------------------------
// --------------------------------------------------------------------------------------------------

//---------------------------------------------------------4.1----------------------------------------------------------
//4.11
let rec countIf p = function
    [] -> 0
    | lst -> let (x::xs) = lst
             let count = countIf p xs
             if p x then count+1 else count
countIf ((>)0) [-1;2;-4;5]

//the warning is about the inner pattern let (x::xs) = lst
// Here lst matches any list including []. But then it tries to destructure it as x::xs which fails for empty lists!
// So the compiler warns that the pattern let (x::xs) = lst doesn't cover the case where lst = [].
let rec countIf2 p = function
    |[] -> 0
    |x::tail ->
             let count = countIf p tail
             if p x then count+1 else count
countIf2 ((>)0) [-1;2;-4;5]
//---------------------------------------------------------4.2----------------------------------------------------------
let countIfA p list =
    let rec aux acc list=
        match list with
        |[] -> acc
        |x::tail ->
            if p x then aux (acc+1) tail
            else aux acc tail
    aux 0 list
countIf2 ((>)0) [-1;2;-4;5]
//---------------------------------------------------------4.3----------------------------------------------------------
let maxList (xs:'a list):'a=
    let rec aux acc lst =
        match lst with
        | [] -> acc
        | x::tail ->
            if x>acc then aux x tail  // recursive call is the LAST thing!
            else aux acc tail
    aux (List.head xs) xs      
maxList ['a';'b';'c';'d']