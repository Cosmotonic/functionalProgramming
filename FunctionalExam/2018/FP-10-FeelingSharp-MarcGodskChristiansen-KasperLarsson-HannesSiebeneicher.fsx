

// Question 1. 
// Question 1. (Polymorphic - a data type is undeclared yet. )
type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>

// Question 1.1

// -- task -- 
// Declare a value ex3 representing the binary tree shown in example 3 above. You may use the template:
// let ex3 = HP(1,HP(2,HP(...HP(4,...
let ex3 = HP(1, HP(2, HP(3, EmptyHP, EmptyHP), HP(5, EmptyHP, EmptyHP)), HP(4, EmptyHP, EmptyHP))

// -- task -- 
// Write the type of the value ex3. Explain why the type is either monomorphic or polymorphic.

// ex3 is monomorphic. The reason is that we have filled in concrete integers (1, 2, 3, 4, 5) into the tree. 
// The type parameter 'a is no longer free — it has been bound to int. There is therefore no flexibility left in the type;
// it can only be Heap<int>.

// -- task -- 
// Declare a value empty representing an empty heap, i.e. a binary tree with only one empty root
// node. The type of the empty value is empty : Heap<’a> when ’a : equality.

let empty : Heap<'a> = EmptyHP

// An empty heap is simply EmptyHP — the constructor that holds no value. 
// The interesting part is the type: by explicitly annotating empty with Heap<'a when 'a : equality>, 
// the type parameter 'a remains free, keeping the value polymorphic. Without the annotation, F# might infer 
// a monomorphic type based on context.

// -- task -- 
// Declare an F# exception named HeapError that can be used to signal an error condition from a function on heaps. 
// The exception should carry a string to be used to describe the error.

exception HeapError of string

// raide (HeapError("there is an error"))

// Question 1.2

// -- 
// Declare a function
// isEmpty : Heap<’a>-> bool when ’a : equality
// that returns true if a heap is the empty heap. For instance isEmpty empty returns true. The
// value empty is defined above
let isEmpty (h : Heap<'a>) : bool =
    match h with
    | EmptyHP -> true
    | _ -> false

isEmpty ex3

let isEmpty_alternative h = 
    h = empty; 


// -- 
// Thesize h of a heap h is the number of non–empty nodes in the binary tree. Declare a function
// size : Heap<’a>-> int when ’a : equality
// that returns the size of a heap. For instance, size ex3 returns 5.
let rec size (h : Heap<'a>) : int =
    match h with
    | EmptyHP -> 0
    | HP(_, l, r) -> 1 + size l + size r

size ex3

// -- 
// Declare a function find h of type
// find : Heap<’a>-> ’a when ’a : equality // means it must support = so it couldnt be two functions
// that returns the minimum value in a non–empty heap, i.e. the root value. For instance find ex3
// returns 1.

let find (h : Heap<'a>) : 'a =
    match h with
    | EmptyHP -> raise (HeapError "find: empty heap")
    | HP(v', _, _) -> v'

find ex3


// -- 
// Declare a function chkHeapProperty h of type
// chkHeapProperty : Heap<’a>-> bool when ’a : comparison
// that returns true if the heap h fulfils the heap property and otherwise false. The empty heap by
// definition fulfils the heap property. For instance chkHeapProperty ex3 returns true.

let rec chkHeapProperty (h : Heap<'a>) : bool =
    match h with
    | EmptyHP -> true
    | HP(v, l, r) ->
        (l = EmptyHP || v <= find l) &&
        (r = EmptyHP || v <= find r) &&
        chkHeapProperty l && chkHeapProperty r 

chkHeapProperty ex3


// Question 1.3

// --
// Declare a function map f h of type 
// map : (’a-> ’b)-> Heap<’a>-> Heap<’b> when ’a : equality and ’b : equality 
// where map f h returns the heap where the function f has been applied on all values in 
// the heap h. You decide, but must explain, what order the function f is applied to the values in the heap. 
// For instance map ((+)1) ex3 returns the heap with all values in ex3 increased by one.

let rec map (f : 'a -> 'b) (h : Heap<'a>) : Heap<'b> =
    match h with
    | EmptyHP -> EmptyHP
    | HP(v, l, r) -> HP(f v, map f l, map f r)


// -- 
// The heap ex3 fulfils the heap property. Give an example of a function f such that mapping f on
// all values in ex3 gives a new heap that does not fulfil the heap property. Given your definition of
// f, show that chkHeapProperty (map f ex3) returns false.

let f x = -x
map f ex3
chkHeapProperty (map f ex3) // returnerer false fordi -1 er større end -2 og -3 der er under den. ulovligt i heap. Root = lowest. 

// Negation reverses the ordering — what was smallest becomes largest. ex3 has 1 as its root because 1 is 
// the smallest value. After negation -1 is the largest value, but still sits at the root. The heap property 
// requires the root to be the smallest, so it is broken immediately.

// Question 2 (30%)
// Question 2 (30%)
// Weshall now consider a binary divide–and–conquer algorithm. We will use the algorithm to implement
// mergesort. You do not need to know how mergesort works to do this.

// Question 2.1
// -- 
// Declare a function genRandoms n of type int-> int[] that returns an array of n random
// integers. The random integers are larger than or equal to 1 and less than 10000. For instance,
// genRandoms 4 mayreturn [|8803;8686;2936;2521|].
// Hint: You can use below to define a generator random of type unit-> int to generate the
// random numbers.
let random = let rnd = System.Random()
             fun () -> rnd.Next(1, 10000)

let genRandoms (n : int) : int[] =
    Array.init n (fun _ -> random())

genRandoms 5

// -- 
// Declare a function genRandomsP n of type int-> int[] that is similar to genRandom except 
// that the numbers are generated in parallel to speed up the process. Hint: You may use the 
// Array.Parallel library as explained in Section 13.6 in the book HR

let genRandomsP (n : int) : int[] =
    Array.Parallel.init n (fun _ -> random())

genRandomsP 100000

// Question 2.2
// Mergesort consists of three separate steps: splitting the remaining unsorted elements in two halves
// (split), identifying when the list has at most one element, and thus is trivially sorted (indivisible)
// and merging two already sorted lists together (merge). We implement each step below.

// -- 
// Declare a function split xs of type ’a list->’alist*’alistwhichtakesalistxs, say
// [e1, . . . , en] and returns two lists with half elements in each: ([e1,...,en/2], [en/2+1,...,en]).
// For instance split [22;746;931;975;200] returns ([22;746],[931;975;200]).
// Define and explain at least three relevant test cases.

let split (xs : 'a list) : 'a list * 'a list =
    let n = List.length xs
    let half = n / 2
    (List.take half xs, List.skip half xs)

split [1;2;3;4;5;6] // = ([1;2], [3;4])


// -- 
// Declare a function indivisible xs of type ’a list-> bool. The function returns true if
// the list is either empty or contains one element only, i.e. the list is trivially sorted; otherwise the
// function returns false. For instance indivisible [23;34;45] returns false.

let indivisible (xs : 'a list) : bool =
    match xs with
    | [] -> true
    | [_] -> true
    | _ -> false

let indivisible_alternative xs = size xs < 2 

// -- 
//  Declare a function merge xs ys of type
//      merge : ’a list * ’a list-> ’a list when ’a : comparison
// that returns the sorted merged list of xs and ys. The function merge can assume the two lists are
// sorted and does not have to check for that. For instance merge ([1;3;4;5],[1;2;7;9])
// returns [1;1;2;3;4;5;7;9]. Define and explain at least three relevant test cases.

let rec merge (xs : 'a list, ys : 'a list) : 'a list =
    match (xs, ys) with
    | ([], ys) -> ys
    | (xs, []) -> xs
    | (x::xrest, y::yrest) ->
        if x <= y
        then x :: merge (xrest, ys)
        else y :: merge (xs, yrest)

merge ([], [1;2;3]) // = [1;2;3]
merge ([1;2;5], [3;4;6]) // = [1;2;3;4;5;6]





// Question 2.3
// The process of solving a problem p using binary divide–and–conquer is to repeatedly divide the problem
// p into problems of half size until the divided problems are indivisible and trivially solved. The divided
// solutions are then merged together until the entire problem p is solved.

let divideAndConquer split merge indivisible p =
    let rec dc p =
        if indivisible p then p
        else
            let (left, right) = split p
            merge (dc left, dc right)
    dc p
divideAndConquer split merge indivisible [22;746;931;975;200]