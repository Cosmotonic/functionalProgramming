

// Assignment 4. 


// Exercise 4.1 first part
let explode (input: string) =
    input.ToCharArray() |> List.ofArray

// Group solution
let explode (s: string) : list<char> = List.ofArray(s.ToCharArray())

explode "kasper"

// Exercise 4.1. second part
let rec explode2 (input: string) = 
    if input.Length = 0 then [] 
    else input.Chars(0) :: explode2(input.Remove(0, 1)) 

explode2 ""

// Group solution 
let rec explode2 (s: string) : List<char>= 
    match s with 
    | "" -> []
    | s -> s.Chars 0 :: explode2 (s.Remove(0, 1))


// explode2 "star"
// = 's' :: explode2 "tar"
// = 's' :: 't' :: explode2 "ar"
// = 's' :: 't' :: 'a' :: explode2 "r"
// = 's' :: 't' :: 'a' :: 'r' :: explode2 ""
// = 's' :: 't' :: 'a' :: 'r' :: []        ← base case returnerer []
// = 's' :: 't' :: 'a' :: ['r']            ← folder sammen på vej op
// = 's' :: 't' :: ['a';'r']
// = 's' :: ['t';'a';'r']
// = ['s';'t';'a';'r']                     ← færdig

// Exercise 4.2 
let rec implode1 (chars: char list) =
    match chars with 
    | [] -> ""
    | x :: xs -> string x + implode1 xs

implode1 ['K'; 'A'; 'S';]

// Group solution 
let implode (inputElements:list<char>): string = 
    List.foldBack(fun element accumulated -> string element + accumulated) inputElements "" 

implode ['K'; 'A'; 'S';] 


// implode ['K';'A';'S']
// = "K" + implode ['A';'S']
// = "K" + "A" + implode ['S']
// = "K" + "A" + "S" + implode []
// = "K" + "A" + "S" + ""
// = "KAS"

// Exercise 4.2. Fold
let implodeRev (charsList: char list) =
    List.fold (fun accumulated x -> string x + accumulated) "" charsList

// implodeRev ['a';'b';'c']
// = fold starter med acc = ""
// = 'a': "a" + "" = "a"
// = 'b': "b" + "a" = "ba"
// = 'c': "c" + "ba" = "cba"

let kasperIntFunc (strings: string list) =
    List.fold (fun acc x -> acc + x ) "" strings 

kasperIntFunc ["kas"; "per"] // remember acc is just a variable that holds the new string. 

// Exercise 4.3 
let toUpper1 (input: string) =
    implode (List.map (fun x -> System.Char.ToUpper x) (explode input))

toUpper1 "kasper"

// Group solution 
let toUpper (s: string): string = 
    implode (List.map System.Char.ToUpper (explode s))

// think of streams in java, remap each element in your input list. 
// input = "hej"
// explode "hej"          = ['h';'e';'j']
// List.map ToUpper       = ['H';'E';'J']
// implode ['H';'E';'J']  = "HEJ"
// fold:   ['a';'b';'c']  →  "abc"   ← mange til én, skal starte et sted
// map:    ['a';'b';'c']  →  ['A';'B';'C']  ← én til én, ingen akkumulator
// map bygger ikke noget op — den transformer bare hvert element og returnerer en ny liste af samme længde


// Exercise 4.3 pt 2 
// forward composition >> :  f >> g betyder g(f(x))
let toUpper2 = explode >> List.map System.Char.ToUpper >> implode
toUpper1 "hej"

// Group solution 
let toUpper22(s:string) : string = 
    (explode >> List.map System.Char.ToUpper >> implode) s

// pipe-forward |> og backward composition 
let toUpper3 input = 
    input |> explode |> List.map System.Char.ToUpper |> implode
toUpper3 "hej"

let toUpper4 (input:string) : string = 
    input |> (explode >> List.map System.Char.ToUpper >> implode)


// 4.4 
// Exercise 4.4 Write a function palindrome:string->bool, 
// so that palindrome s returns true if the string s is a palindrome; 
// otherwise false. Astring is called a palindrome if it is identical 
// to the reversed string, eg, “Anna” is a palindrome but “Ann” is not. 
// The function is not case sensitive.

let padlidrome (s:string) bool = 
    s // first input
    |> fun s -> s.ToLower()
    |> explode
    |> implodeRev
    |> fun x -> x = s


// 4.5 - ackerman 
//  
//              ⎧ n + 1                  if m = 0
// A(m, n) =    ⎨ A(m - 1, 1)            if m > 0 and n = 0
//              ⎩ A(m - 1, A(m, n - 1))  if m > 0 and n > 0
let rec ack (m: int, n: int) : int =
    match m, n with
    | 0, n -> n + 1
    | m, 0 when m > 0 -> ack (m - 1, 1) 
    | _, _ when m > 0 && n > 0 -> ack (m - 1, ack (m, n - 1))

// Test
ack (3, 11) 
//val ack: m: int * n: int -> int
//val it: int = 16381

let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
        (res, finish- start);

time (fun () -> ack (3, 11))

let timeArg f a = 
    time (fun () -> f a)

timeArg ack (3, 11)