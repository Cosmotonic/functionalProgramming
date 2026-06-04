

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

// Explaination: In F# you can define custom exceptions using the exception keyword. By writing of string we give the exception 
// a field that carries a text message. It can then be raised with e.g. raise (HeapError "something went wrong").


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

//  Explaination: The function uses pattern matching to distinguish between the two constructors of the Heap type.
//  If the heap matches EmptyHP it returns true. The underscore _ catches all other 
//  cases — i.e. HP(v, l, r) — and returns false.

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

// Explaination: Because the heap property guarantees that the root is always the minimum value, we do not need to
// search through the tree — we simply return the root value directly. If the heap is empty there is 
// no root value, so we raise a HeapError. We use _ to ignore the two subtrees as we have no need for them.


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

// Explaination: The heap property states that a node must always be less than or equal to its children. 
// For each non-empty node with value v we therefore check: is v less than or equal to the value of the 
// left child? And is v less than or equal to the value of the right child? We use find to retrieve the 
// child's root value, but skip the comparison if the child is EmptyHP. We then recursively check that the 
// same property holds for each subtree. This guarantees that the property holds for all nodes in the entire 
// tree, not just the root.


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

// Next assignment runs the code. 

// The function traverses the tree in pre-order — meaning we apply f to the root first, then recursively 
// to the left subtree, then the right subtree. This choice is natural because our datatype is defined as 
// root-left-right, and it gives the most direct implementation. It is important to note that map does not 
// guarantee that the heap property is preserved in the new heap — if f is not monotone (e.g. a function 
// that reverses the order) the ordering between parents and children may be broken.


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

// Explaination: Array.Parallel.init works exactly like Array.init — but elements are computed in 
// parallel across multiple threads. This means multiple random() calls can happen simultaneously. 
// This is safe here because System.Random in newer .NET versions is thread-safe. For large n this can 
// give a measurable speedup.


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

// Explaination: We find the length of the list, divide by 2 (integer division, rounds down), 
// and use List.take and List.skip to cut the list at that point. List.take half 
// xs gives the first half elements, List.skip half xs gives the rest. The test cases 
// cover edge cases: empty list, even count, odd count, and single element.

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

// Explaination: A list with zero or one element is by definition sorted — there is nothing to compare or swap. 
// The function uses pattern matching to distinguish between the three cases: empty list, list with exactly 
// one element ([_]), and all other lists. The name "indivisible" relates to the context — a list that cannot 
// be meaningfully divided for further sorting.


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

// We compare the two front elements x and y. If x is less than or equal to y, 
// x is placed at the front of the result list. We then call merge again with the rest of xs (xrest) 
// and the entirety of ys unchanged — to once again compare the next elements. This way we build the 
// sorted list one element at a time. When one list is exhausted we simply append the remaining list 
// directly — it is already sorted.



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

// Explaination:  The function is a higher-order function — it takes three functions as arguments
// and combines them into one generic algorithm. If the problem p is indivisible it 
// is returned directly as the solution. Otherwise p is divided into two halves with split, 
// both halves are solved recursively with dc, and the two solutions are merged together with merge.
// Merge sort is achieved by inserting our concrete split, merge and indivisible functions as arguments.


// Question 3 (20%)
// Question 3 (20%)
// In this question we work with sequences as covered in Chapter 11 in HR.

// -- 
// Declare an infinite sequence triNum of triangular numbers. The n'th triangular number 
// is defined by the formula x_n = n(n+1)/2. The sequence starts at index n=0 and gives [0;1;3;6;10;...].

let triNum : seq<int> = Seq.initInfinite (fun n -> n * (n + 1) / 2)

triNum 

// Explaination: Seq.initInfinite f creates an infinite sequence where the n'th element is computed by 
// calling f n. Sequences in F# are lazy — elements are not computed upfront, but only when requested. 
// This is precisely what makes it possible to have an infinite sequence without running out of memory.
// We simply insert the formula n * (n+1) / 2 directly.

// -- 
// Declare a cached version triNumC of triNum such that already computed elements are cached.
// The type of triNumC is seq<int>.

let triNumC : seq<int> = Seq.cache triNum

// Explanation: A normal lazy sequence recomputes each element every time it is accessed. Seq.cache wraps the 
// sequence so computed elements are stored in memory — the next time the same element is requested
// it is retrieved directly from the cache instead of being recomputed. This is a tradeoff between 
// time (faster lookup) and memory (elements are stored).


// Question 3.2
// The function filterOddIndex s filters out all elements ei of the sequence s where i is odd. The
// function declaration is based on the assumption that the input sequence s is infinite but unfortunately
// goes into an infinite loop. For instance filterOddIndex triNum never terminates.
// let rec filterOddIndex s =
//     Seq.append (Seq.singleton (Seq.item 0 s))
//     (filterOddIndex (Seq.skip 2 s))

// --
// Declare your own version myFilterOddIndex similar to filterOddIndex except that it
// does not enter an infinite loop but returns the intended sequence.
// The sequence for myFilterOddIndex triNum is seq [0;3;10;21;...].

let rec myFilterOddIndex s =
    seq {
        yield Seq.item 0 s
        if not (Seq.isEmpty (Seq.skip 2 s)) then
            yield! myFilterOddIndex (Seq.skip 2 s)
    }

myFilterOddIndex triNum |> Seq.take 70
myFilterOddIndex triNum |> Seq.take 70 |> Seq.toList

// Explanation: The original fails because Seq.append eagerly evaluates both arguments, 
// triggering infinite recursion before producing any element. By using a sequence 
// expression with yield and yield!, the recursion becomes lazy — the next element is only 
// computed when requested. We produce element at index 0, skip 2 positions, 
// and recurse on the remainder.


// Question 3.3
// The sequence library Seq contains a number of functions to manipulate sequences, see Table 11.1 in
// HR. One such function is Seq.zip s1 s2 of type
//          (seq<’a>-> seq<’b>-> seq<’a * ’b>)
// For instance executing Seq.zip triNum triNum returns the value
//      seq [(0, 0); (1, 1); (3, 3); (6, 6); ...]

// --
// Declare a function seqZip of type (seq<'a> -> seq<'b> -> seq<'a * 'b>)
// that works the same as Seq.zip using sequence expressions.

let rec seqZip s1 s2 =
    seq {
        let e1 = Seq.item 0 s1
        let e2 = Seq.item 0 s2
        yield (e1, e2)
        yield! seqZip (Seq.skip 1 s1) (Seq.skip 1 s2)
    }

seqZip triNum triNum |> Seq.take 10 |> Seq.toList

// seqZip uses a sequence expression to lazily pair up elements from two sequences.
// We take the first element from each sequence, yield them as a tuple, then
// recursively yield the rest. The use of yield! ensures the recursion is lazy —
// elements are only computed on demand, making it safe to use with infinite sequences.


// Question 4
// Question 4
// We now consider an internal domain specific language (DSL) called Fig to be used for specifying figures
// constructed from basic figures, that is, circles and lines. The DSL contains constructors for forming
// a collection of figures (constructor Combine), for specifying a move of a figure by a given offset
// (constructor Move) and for naming and referencing figures (constructors Label and Ref). 

//  Theexception FigError represents an error condition from a function in the library.
//  Thetype Point represents a point (x,y) in the two dimensional space.
//  Thetype Fig represents the DSL for figures– Circle(p,r) is the circle with center p and radius r.– Line(p1,p2) is the line between the two points p1 and p2– Move (dx,dy,fig) denotes the figure obtained from fig by moving the figures contained in
// fig as specified by dx and dy.
// – Combinefigs is the collection of figures in figs.
// – Label(lab,fig) gives the fig a name lab. We assume fig does not contain references (Ref) such that cyclic structures are avoided.
// – Reflab references the figure with name lab assuming it exists.

exception FigError of string

type Point = P of double * double

type Fig =
    | Circle of Point * double
    | Line of Point * Point
    | Move of double * double * Fig
    | Combine of Fig list
    | Label of string * Fig
    | Ref of string


// Question 4.1
// --
// Declare an F# value rectEx of type Fig that represents a rectangle
// with the four points (-1,1), (1,1), (1,-1) and (-1,-1).

let rectEx =
    Combine [
        Line(P(-1.0, 1.0), P( 1.0,  1.0))
        Line(P( 1.0, 1.0), P( 1.0, -1.0))
        Line(P( 1.0,-1.0), P(-1.0, -1.0))
        Line(P(-1.0,-1.0), P(-1.0,  1.0))
    ]

rectEx

// rectEx is simply four lines connecting the four corners of the rectangle
// in order: top-left -> top-right -> bottom-right -> bottom-left -> top-left.


// --
// Declare a function rect (x1,y1) (x2,y2) of type
// double * double -> double * double -> Fig
// that given two orthogonal coordinates returns a figure representing
// the rectangle by its four sides.

let rect (x1, y1) (x2, y2) =
    Combine [
        Line(P(x1, y1), P(x2, y1))
        Line(P(x2, y1), P(x2, y2))
        Line(P(x2, y2), P(x1, y2))
        Line(P(x1, y2), P(x1, y1))
    ]

rect (-2.0, 1.0) (1.0, -1.0)

// Given two corner points (x1,y1) top-left and (x2,y2) bottom-right, we derive
// the four corners and connect them with lines. The two remaining corners are
// (x2,y1) top-right and (x1,y2) bottom-left.


// Question 4.2 FIG = FIGURE
// Consider the F# value figEx02 consisting of a labeled circle "c"
// which is referenced twice. The referenced circles are moved such that
// we obtain a figure like the one to the right.
let figEx02 = 
    Combine [Label("c",Circle(P(0.0,0.0),1.0)); // vi laver et label C som refere til cirklen p. 
         Move(1.0,1.0, Ref "c"); // huske den bruger REF her. Så der er ikke nogen cirkel nendu. 
         Move(2.0,2.0, Ref "c")]

// --
// Declare a function buildEnv fig of type Fig -> Map<string,Fig>
// that traverses fig and builds an environment mapping labels to figures.

let rec buildEnv (fig : Fig) : Map<string, Fig> =
    match fig with
    | Circle _ -> Map.empty
    | Line _   -> Map.empty
    | Ref _    -> Map.empty
    | Move(_, _, f)   -> buildEnv f
    | Label(lab, f)   -> Map.add lab f (buildEnv f)
    | Combine figs    -> List.fold (fun env f -> Map.fold (fun e k v -> Map.add k v e) env (buildEnv f)) Map.empty figs

let envEx02 = buildEnv figEx02 // map [("c",Circle(P(0.0,0.0),1.0))]

// buildEnv traverses the entire figure tree recursively. Only Label nodes contribute
// to the environment — they add a mapping from the label string to the figure.
// For Combine we fold over all subfigures and merge their environments together.
// Circle, Line and Ref contribute nothing to the environment.


// Question 4.3
// Given a figure fig and an environment env mapping labels to figures, we can substitute referenced figures
// with the actual figures.

// --
// Declare a function substFigRefs env fig of type Map<string,Fig> -> Fig -> Fig
// that substitutes all references with actual figures, removing both Ref and Label constructors.

let rec substFigRefs (env : Map<string, Fig>) (fig : Fig) : Fig =
    match fig with
    | Circle _         -> fig
    | Line _           -> fig
    | Ref lab          ->
    match Map.tryFind lab env with
        | Some f -> f
        | None   -> raise (FigError ("substFigRefs: unknown label " + lab))
    | Label(_, f)      -> substFigRefs env f
    | Move(dx, dy, f)  -> Move(dx, dy, substFigRefs env f)
    | Combine figs     -> Combine (List.map (substFigRefs env) figs)

let figEx03 =
    Combine [Label("c", Circle(P(0.0,0.0), 1.0))
             Move(1.0, 1.0, Ref "c")
             Move(2.0, 2.0, Ref "c")]

let envEx03    = buildEnv figEx03
let substEx03  = substFigRefs envEx03 figEx03 // FØR : Ref "c"   →  EFTER:  Circle(P(0.0,0.0), 1.0)


// substFigRefs recursively traverses the figure. When it encounters a Ref it looks up
// the label in the environment and replaces it with the actual figure. When it encounters
// a Label it discards the label and recurses on the inner figure. All other constructors
// are traversed recursively. If a Ref references an unknown label we raise a FigError.


// Question 4.4
// We now assume that figures do not contain labels and references. For such figures, we can remove the
// Move constructors by updating the positions of the circles and lines. We thus obtain a figure consisting
// of Combine, Circle and Line constructors only.

// --
// Declare a function reduceMove fig of type Fig -> Fig that removes all Move constructors
// by updating the positions of circles and lines directly.

let rec reduceMove (fig : Fig) : Fig =
    reduceMoveDx 0.0 0.0 fig

and reduceMoveDx (dx : double) (dy : double) (fig : Fig) : Fig =
    match fig with
    | Circle(P(x, y), r)          -> Circle(P(x + dx, y + dy), r)
    | Line(P(x1,y1), P(x2,y2))    -> Line(P(x1+dx, y1+dy), P(x2+dx, y2+dy))
    | Move(ddx, ddy, f)           -> reduceMoveDx (dx + ddx) (dy + ddy) f
    | Combine figs                -> Combine (List.map (reduceMoveDx dx dy) figs)
    | Label _                     -> raise (FigError "reduceMove: unexpected Label")
    | Ref _                       -> raise (FigError "reduceMove: unexpected Ref")

let reduceEx03 = reduceMove substEx03

// reduceMove works by accumulating the total offset (dx, dy) as it traverses the tree.
// When it encounters a Move(ddx, ddy, f) it adds the offsets to the accumulated total
// and recurses. When it reaches a Circle or Line it applies the total accumulated offset
// directly to the coordinates. This way all Move constructors are eliminated and positions
// are updated in place. Label and Ref should not appear (we call this after substFigRefs)
// so we raise a FigError if they do.


