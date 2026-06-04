
(*
Question 1 (30%)


The example above is declared as
let psEx = PrioritySet ["a";"b";"c"]
Question 1.1
Consider the following elements and assume they are inserted in an empty priority set in this order: "a",
"q", "a", "b", "b", "q", "d", "a".
• Declare a value priSetEx, being the result of inserting the elements above according to the
definition of a priority set.
• What is the type of the value priSetEx.
• Declare a value empty representing an empty priority set, i.e., priority set with no elements.

*)
type PrioritySet<'a when 'a: equality> = PrioritySet of List<'a>

let priSetEx = PrioritySet ["a"; "q"; "b"; "d"]
// type of the value priSetEx.   PrioritySet<string>
let empty = PrioritySet []

(*
Question 1.2
• Declare a function
isEmpty : PrioritySet<’a> -> bool when ’a : equality
that returns true if a priority set is the empty set. For instance isEmpty(empty) returns true.
The value empty is defined above.
• The size of a priority set is the number of elements in the set. Declare a function
2
BSWU BFNP–1413003U IT University, F2017
size : PrioritySet<’a> -> int when ’a : equality
that returns the size of a priority set. For instance, size psEx returns 3.
• Declare a function contains e ps of type
contains : ’a -> PrioritySet<’a> -> bool when ’a : equality
that returns true if the priority set ps contains an element e. For instance contains "b" psEx
returns true.
• Declare a function getPN e ps of type
getPN : ’a -> PrioritySet<’a> -> int when ’a : equality
that returns the priority number of element e if exists in priority set ps. Otherwise raises an error
exception (failwith). For instance getPN "a" psEx returns 1.

*)

// let isEmpty : PrioritySet<’a> -> bool when ’a : equality
let isEmpty (ps : PrioritySet<'a>) : bool =
    match ps with
    | PrioritySet [] -> true
    | PrioritySet _ -> false

isEmpty priSetEx
isEmpty empty

// size : PrioritySet<’a> -> int when ’a : equality
let rec size (ps : PrioritySet<'a>) : int = 
    match ps with 
    | PrioritySet [] -> 0
    | PrioritySet (head::tail) -> 1 + size (PrioritySet tail)
// or 
let size_short (PrioritySet xs) = List.length xs

size priSetEx
size empty

// contains : ’a -> PrioritySet<’a> -> bool when ’a : equality
let rec contains input ps =
    match ps with
    | PrioritySet [] -> false                       // Basistilfælde: Listen er tom, så elementet findes ikke.
    | PrioritySet (x::xs) ->                        // Vi splitter listen i 'hoved' (x) og 'hale' (xs).
        if x = input then true                      // Fundet!
        else contains input (PrioritySet xs)        // Prøv igen med resten af mængden.
// Or
let contains_short input (PrioritySet xs) = 
    List.contains input xs; 

contains "a" priSetEx
contains_short "a" priSetEx


// getPN : ’a -> PrioritySet<’a> -> int when ’a : equality
let getPN input (PrioritySet xs) = 
    let rec find i list = 
        match list with 
        | [] -> failwith ("Element not found in list")
        | x::tail -> 
            if x = input then i 
            else find (i+1) tail
    find 1 xs // my find initializer

// OR 
let getPN_alternative e (PrioritySet xs) =
    match List.tryFindIndex (fun x -> x = e) xs with
    | Some i -> i + 1
    | None   -> failwith "Element not found"

getPN_alternative "b" priSetEx
getPN "b" priSetEx


(*
Question 1.3
• Declare a function remove e ps of type
remove : ’a -> PrioritySet<’a> -> PrioritySet<’a> when ’a : equality
that removes element e from the priority set ps and returns a new priority set. Nothing changes if
e does not exists in ps. For instance, remove "b" psEx returns the priority set PrioritySet
["a";"c"].
• Declare a function
add : ’a -> PrioritySet<’a> -> PrioritySet<’a> when ’a : equality
where add e ps returns the priority set ps with the element e added with lowest priority (highest
priority number) unless already in the set ps. Adding element h to priority set {a1, b2, c3} gives
the priority set {a1, b2, c3, h4}. Adding element b to {a1, b2, c3} gives the unchanged priority set
{a1, b2, c3}.
*)


// remove : ’a -> PrioritySet<’a> -> PrioritySet<’a> when ’a : equality
let remove input (PrioritySet xs) =
    let rec removeFromList list =
        match list with
        | [] -> []                          // Opgavekrav: Returnér tom liste, hvis intet findes
        | x::tail -> 
            if x = input then 
                tail                        // Hop over 'x' og returnér resten (færdig)
            else 
                x :: removeFromList tail    // Behold 'x' og led videre i resten
    PrioritySet (removeFromList xs)         // Husk at pakke resultatet ind i æsken

let rmv1 = remove "a" priSetEx


// add : ’a -> PrioritySet<’a> -> PrioritySet<’a> when ’a : equality

let add input (PrioritySet xs) = 
    let rec addToList list = 
        match list with 
        | [] -> [input]
        | x::tail -> 
            if x = input then failwith ("item already exists")
            else x :: addToList tail
    PrioritySet (addToList xs) // my find initializer packaged into prioritySet. 

let ps1 = add "l" priSetEx
let ps2 = add "t" ps1
