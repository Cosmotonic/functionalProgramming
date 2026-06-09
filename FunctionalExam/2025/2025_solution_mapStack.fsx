type stack<'b> = list<'b>
type mapStack<'a,'b> = MapStack of list<'a*stack<'b>>

// The map–stack ex1 above may be implemented as follows:
let ex1 = MapStack [('1',[3;1;1]); ('2',[1;3;1]); ('3',[1;1;3]); ('4',[3;3;1]); ('5',[2;3;4]); ('6',[2;1;2])]

// Question 1.1
// Declare a MapStack for the other table
let ex2 = MapStack [("Hey", ['H'; 'E'; 'Y']); ("there", ['t'; 'h'; 'e'; 'r'; 'e']); ("", [])]
// Ex2 type: string * char list - its a monomorphic not poloymorphic

//Declare an F# function empty() of type unit-> mapStack<’a,’b> that evaluates to an empty map–stack.
// tag empty declare an empty 
let empty () : mapStack<'a,'b> = MapStack []

// Declare an F# function numKeys m of type mapStack<’a,’b>-> int that evaluates to the
// number of keys in the map–stack m. For instance numKeys ex1 evaluates to 6
let rec numKeys m : int = 
    match m with 
    | MapStack [] -> 0 
    | MapStack (x::rest) -> 1 + numKeys (MapStack rest)

numKeys ex1
numKeys ex2

let numKeys_alternative (MapStack xs) = List.length xs
numKeys_alternative ex1

// Declare an F# function numValues m of type mapStack<’a,’b>-> int that evaluates to
// the number of values in the stacks in the map–stack m. For instance numValues ex1 evaluates to 18. (counting, count, open list, get values)

let rec numValues m : int =
    match m with
    | MapStack [] -> 0
    | MapStack ((_, values)::rest) ->
        List.length values + numValues (MapStack rest)

numValues ex2

let numValues_alternative (MapStack xs) =
    xs |> List.sumBy (fun (_, values) -> List.length values)
numValues_alternative ex2


// Question 1.2 
// Question 1.2 
// Question 1.2 

// Declare an F# function pop s of type ’a list-> ’a*’a list that evaluates to a pair (sh,st)
// where s = sh :: st. That is, sh is the head element and st is the tail of the list s. In case s is
// empty, an exception, System.Exception, is thrown containing an error message: “Can’t pop
// empty stack”. For instance pop [1;2;3] evaluates to (1,[2;3]).

let pop s = 
    match s with 
    | [] -> raise (System.Exception ("Can't pop empty stack"))
    | h::t -> (h, t) 

pop [1;2;3]

// Declare an F# function remove k l of type
// ’a-> (’a * ’b) list-> (’a * ’b) list when ’a: equality
// that evaluates to a list l′ equal to l except that a pair (k′,v′) in list l where k = k′ is removed. For
// instance remove 1[(1,"v1");(2,"v2");(3,"v3")] evaluatesto[(2,"v2");(3,"v3")].

// Explain what the constraint when ’a: equality in the type for remove means and why it is
// necessary (Tag: build list, empty list, remove list)
// ANSWER: It is necessary because not all values are compareable. 

let myList: (int * string) list = [(1,"v1");(2,"v2");(3,"v3")]

let rec remove k l = 
    match l with 
    | [] -> []
    | (key, value)::rest -> 
        if key = k then remove k rest // this is the recursiveness before the roll-back-back 
        else (key, value)::remove k rest // If no match, add current to new list and check the rest of the input list. 

remove 2 myList

// alternative solution 
let remove_alternative k m = List.filter (fun (k', _) -> k' <> k) m
remove 2 myList

// Declare an F# function mapPush k v m of type
// ’a-> ’b-> mapStack<’a,’b>-> mapStack<’a,’b> when ’a: equality
// that evaluates to a new map–stack, where the value v is pushed on the stack that the key k maps
// to in the map–stack m. If the key k does not exists in m, then the pair (k,[v]) is added to m. For
// instance, mapPush ’1’ 2 ex1  evaluates to: 
//  MapStack [('1', [2; 3; 1; 1]); ('2', [1; 3; 1]); ('3', [1; 1; 3]); ('4', [3; 3; 1]); ('5', [2; 3; 4]); ('6', [2; 1; 2])]
// Tag: Add element to list

let rec mapPush k v m =
    match m with
    | MapStack [] -> MapStack [(k, [v])]                    // The mapStack is empty. We know the value doesnt exist so we just add it. Recursively too. 
    | MapStack ((key, values)::rest) ->                     // rest=(key, value);(key,value)...
        if key = k then MapStack ((k, v::values) :: rest)   // Key exist, the funciton returns a new MapStack with the new value included. 
        else                                                // We have checked all current keys ad input k does not exist, 
            let restMapStack = mapPush k v (MapStack rest)  // recurse call with the remaing MapStack, return the mapStack of the rest
            match restMapStack with                         // Happens only AFTER the restMapStack is handled for ALL iterations. 
            | MapStack newRest -> MapStack ((key, values) :: newRest) // At each return, we add that call’s (key, values) in front of the result, so the list is rebuilt from the end back to the start, including the new (k, v) where it belongs.

mapPush '1' 5 ex1 
mapPush '6' 9 (MapStack [('2',[2]); ('3',[2]); ('8',[2]); ('7',[2])])


// alternative solution 
let mapPush_alternative k v (MapStack m) =
    MapStack (match List.tryFind (fun (k', _) -> k = k') m with
              | Some (_, ys) -> (k, v :: ys) :: (remove k m)
              | None -> (k, [v]) :: m)

              
             
(*
Declare an F# function mapPop k m of type
’a-> mapStack<’a,’b>-> ’b * mapStack<’a,’b> when ’a: equality
that evaluates to the value (v,m′) if k exists in the map–stack m and v is the head element
of the stack, s, that the key k maps to in m. The map–stack m′ is equal to m except that
the value v has been popped from the stack s. In case a key is not found in m, an exception,
System.Exception, with error message “Can’t find key.” is thrown. In case the affiliated
stack s is empty, an exception, System.Exception, with error message “Can’t pop empty
stack” is thrown. For instance, mapPop ’3’ ex1 may evaluate to
(1, MapStack [(’3’, [1; 3]); (’1’, [3; 1; 1]); (’2’, [1; 3; 1]); (’4’, [3; 3; 1]); (’5’, [2; 3; 4]); (’6’, [2; 1; 2])])

*)

let rec mapPop k m = 
    match m with
    | MapStack [] -> raise (System.Exception ("Can't find key"))
    | MapStack ((key, values)::rest) ->  
        if key = k then match values with
                        | [] ->  raise (System.Exception ("Can't pop empty stack"))
                        | x::restValues -> (x, MapStack ((key, restValues) :: rest))
        else 
            let (v, MapStack newRest) = mapPop k (MapStack rest)  
            (v, MapStack ((key, values) :: newRest)) // find noget dybt i strukturen - og byg det hele op igen på vej tilbage

let newPop = mapPop '3' ex1 


(*

Declare an F# function map f m of type (’a-> ’b-> ’c)-> mapStack<’a,’b>->
mapStack<’a,’c>, that applies the function f on all elements in all the stacks in the map
stack m. For instance, map (fun _ v-> v+1) ex1 may evaluate to
MapStack
[(’1’, [4; 2; 2]); (’2’, [2; 4; 2]); (’3’, [2; 2; 4]);
(’4’, [4; 4; 2]); (’5’, [3; 4; 5]); (’6’, [3; 2; 3])]

TAG: list stream. Change whole list with same function. 
*)

// Question 1.3

(*
Declare an F# function map f m of type (’a-> ’b-> ’c)-> mapStack<’a,’b>->
mapStack<’a,’c>, that applies the function f on all elements in all the stacks in the map
stack m. For instance, map (fun _ v-> v+1) ex1 may evaluate to

Tag: higher order function, higher-order, function as inputs. 
*)

let rec map f m = 
    match m with 
    | MapStack [] -> MapStack []
    | MapStack ((k, v)::rest) -> 
        let mappedValues = List.map (fun value -> f k value) v // value er én værdi fra listen v, én ad gangen.
        let (MapStack newRest) = map f (MapStack rest)
        MapStack ((k, mappedValues) :: newRest) // rebuilds the list recursively from the back, placeing previous values in the front. 

let runMapper = map (fun _ v -> v + 1) ex1


(*
 Declare an F#function fold f e m of type(’a->’b->’c->’a)->’a->mapStack<’b,’c>-> ’a that 
 folds the function f over all entries in the stacks in the map–stack m starting 
 with initial value e. For instance, fold (fun str _ v-> str + v.ToString()) "" ex2 may
evaluate to "Heythere".
*)

Slet rec fold f e m = 
    match m with 
    | MapStack [] -> e
    | MapStack ((k, v)::rest) -> 
        let foldedList = List.fold ( fun acc value -> f acc k value) e v    // let foldedLists = List.fold (fun value -> f k value) v
        fold f foldedList (MapStack rest)
        
fold (fun str _ v -> str + v.ToString()) "" ex2 


(* Question 2 (30%)
We consider FAKElite, a simplified internal domain specific language for automating build and release
pipelines. FAKElite consists of actions, build steps, and the ability to execute build steps consisting of
one or more actions
*)
// Actions
let log s = printfn "--> %s" s

let clean() = log "cleaning..."
let build() = log "building..."
let unitTest() = log "unit testing..."
let releaseTest() = log "release testing..."
let deploy() = log "deploying..."

let cpFile src dst =
    fun () -> log ("copying file from " + src + " to " + dst + " ...")

cpFile "foo.txt" "Bar/"

let chain a1 a2 =
    fun() -> List.iter (fun f -> f()) [a1; a2]

let chain a1 a2 =
    fun() -> a1(); a2()

let allTest = chain unitTest releaseTest

let (++) a1 a2 = chain a1 a2


type buildStep = {
    name: string
    action: unit -> unit
    deps: string list
}

let mkBuildStep name action = {
    name = name
    action = action
    deps = []
}


let cleanBS = mkBuildStep "clean" clean
let buildBS = mkBuildStep "build" build


let dependsOn name ({deps=deps} as bs : buildStep) =
    {bs with deps = name :: deps}

let buildBS' = dependsOn "clean" buildBS


let bsEnv =
    Map.ofList [
        ("clean", cleanBS)
        ("build", buildBS')
    ]


let lookupBS name env =
    match Map.tryFind name env with
    | None ->
        failwith ("Failed to find build step " + name + ".")
    | Some bs ->
        bs


lookupBS "clean" bsEnv


let pushUnique x xs =
    if List.exists ((=) x) xs then xs
    else x :: xs


// Get build step order without execution
let getDeps name env =
    let rec loop name bss =
        let bs = lookupBS name env
        let bss =
            List.fold (fun bss name -> loop name bss) bss bs.deps
        pushUnique bs.name bss
    List.rev (loop name [])


getDeps "clean" bsEnv
getDeps "build" bsEnv




(* 
Question 3 (20%)
Question 3.1
Consider the function triangular, defined as triangular(n) = n(n+1) / 2  for any n > 0.
• Declare an F# function triangular n of type int-> int that implements the triangular
formula above. The function must throw exception System.Exception with message “Error,
negative argument” in case the triangular formula is not defined for the given n. For instance,
triangular 3 evaluates to 6
*)

let triangular n = 
    if n < 0 then raise (System.Exception ("Can't find key"))
    else 
        (n * ( n + 1 ))/ 2 

triangular 7

(* 
 Declare an F# sequence triangularSeq of type int seq containing the triangular numbers
as defined above. The first sequence element holds the triangular number for the smallest n
where the formula is defined. For instance, Seq.take 4 triangularSeq evaluates to seq
[1;3;6;10].

TAG: lazy sequence
"The first sequence element holds the triangular number for the smallest n where the formula is defined"
Menneskesprog: Det første tal i listen skal være det første gyldige trekanttal
*)

let triangularSeq = Seq.initInfinite (fun n -> (n + 1) * (n + 2) / 2)

Seq.take 4 triangularSeq

(* 
DeclareanF#sequencetriangularSeq’oftypeintseqthatisacachedversionoftriangularSeq
above.
*)
let triangularSeqCached = Seq.cache triangularSeq


(*
uestion 3.2
Consider the recursive definition of Catalan numbers, C

Analyse the code for catMapSlow and explain why it is slow.
Hint: The compiler issues below warning when compiling the function, see comment above for
the exact line:

warning FS0020: The result of this expression has type ’Map<int,int>’ and is implic
itly ignored. Consider using ’ignore’ to discard this value explicitly, e.g. ’expr |>
ignore’, or ’let’ to bind the result to a name, e.g. ’let result = expr’.
*)

let catMapSlow n =
    let cache = Map.empty
    if n < 0 then failwith "Error, negative argument"
    else
        let rec cat n =
            if n = 0 then 1
            else
                if Map.containsKey n cache then cache[n] // we dont store the cache - this is always false. 
                else
                    let sum = List.fold (fun s i-> s + cat i * cat (n-1-i)) 0 [0..n-1]
                    Map.add n sum cache // Map.add returns a new map with (n, sum), but since the result is not stored, the cache is not updated. We dont store the value in a let 
                    sum
        cat n

catMapSlow 17
// ANSWER: We dont utilize the cache because we never create a new cache with previous caches.(its not additive)



(*
Declare an F# function catArray n of type int-> int similar to catMapSlow except that
a mutable array, type int array, is used instead of the map. Show the result of evaluating
catArray 17 which should give same result as above.
*)
let catArray n =                              // define function taking n
    if n < 0 then failwith "Error, negative argument" // guard against negative input
    else
        let cache = Array.create (n + 1) -1   // create mutable array for caching, initialized with -1 in all indexes
        let rec cat k =                       // recursive helper function
            if k = 0 then 1                  // base case: Catalan(0) = 1
            elif cache[k] <> -1 then cache[k] // if value already computed (its not -1 as initialized above.) return cached value (in f# you dont use != you use <> )  
            else
                let sum =                    // compute Catalan number for k
                    [0..k-1]                 // iterate over required indices
                    |> List.fold (fun s i -> s + cat i * cat (k - 1 - i)) 0 // sum recursive products
                cache[k] <- sum              // store computed value in cache (mutation)
                sum                          // return computed value

        cat n                                // start recursion with input n

catArray 17 

(* 
Question 4 (20%)
Consider below F# declaration: *) 
let rec countIf p = function
    []-> 0
    | lst-> let (x::xs) = lst
            let count = countIf p xs // recursive call. 
            if p x then count+1 else count

(* 
The type of countIf is (’a-> bool)-> ’a list-> int. The expression countIf ((>)0)
[-1;2;-4;5] evaluates to 2, that is, the argument list contains two numbers larger than 0.
*) // p = predicate

countIf ((>) 0) [-1;2;-4;5;-2] // ((>) 0) // bliver til: fun x -> 0 > x

(*
Question 4.1
• Explain why the F# compiler reports below warning when compiling the function countIf.
warning FS0025: Incomplete pattern matches on this expression. For example, the
value ’[]’ may indicate a case not covered by the pattern(s).
• Write a version of countIf, called countIf2, that compiles without warnings and evaluates
to exact same results as countIf for same arguments.
*)

// We are not pattern matching. Hence the list can be empty in the above example. 
let rec countIf2 p lst =
    match lst with
    | [] -> 0
    | x::xs ->
        let count = countIf2 p xs
        if p x then count + 1 else count

countIf2 ((>) 0) [-1;2;-4;5;-2] // ((>) 0) // bliver til: fun x -> 0 > x


(* 
The function countIf is not tail recursive. Declare a tail-recursive variant, countIfA, that evaluates
to exact same results as countIf for same arguments. Show the result of evaluating countIfA
((>)0) [-1;2;-4;5].

TAG: tail recurion kræver accumulator

*)

let countIfA p lst =
    let rec loop acc lst =
        match lst with
        | [] -> acc
        | x::xs ->
            if p x then loop (acc + 1) xs // hvis predicate er sandt så add en til accumulator
            else loop acc xs              // ellers gå videre til næste værdi uden accumulator increment 
    loop 0 lst

// counts negative numbers? 
countIfA ((>) 0) [-1;2;-4;5;6]


(*
Declare a tail–recursive F# function maxList xs of type ’a list-> ’a when ’a: comparison
that evaluates to the maximum element in the list xs. For instance, maxList [’a’;’b’;’d’] eval
uates to ’d’.

TAG: Comparator, compare list, compare values. 
*)

let maxList xs =
    match xs with
    | [] -> failwith "empty list"
    | x::xs ->
        let rec loop currentMax lst =
            match lst with
            | [] -> currentMax
            | y::ys ->
                if y > currentMax then loop y ys
                else loop currentMax ys
        loop x xs


maxList ['a';'b';'d']   // 'd'
maxList [1; 4; 23; 3]   // 'd'