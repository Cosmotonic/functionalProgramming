// -------------------------------------------------------------------------
// "Declare an F# function noRemainderP m n of type int-> int-> bool that 
// returns true if m modulo n is 0, that is m % n is 0. For instance, 
// noRemainderP 10 2 evaluates to true and noRemainderP 10 3 evaluates to false.
// The noRemainderP function is not recursive."
// -------------------------------------------------------------------------
let noRemainderP m n = m % n = 0

printfn "%b" (noRemainderP 10 2)  // true
printfn "%b" (noRemainderP 10 3)  // false


// -------------------------------------------------------------------------
// "Declare an F# function checkNumber n m of type int-> int-> bool that 
// evaluates to true if noRemainderP m l is true for all 1 ≤ l ≤ n; 
// otherwise checkNumber evaluates to false. For instance, checkNumber 10 2520 
// evaluates to true and checkNumber 11 2520 evaluates to false."
// -------------------------------------------------------------------------
let checkNumber n m = List.forall (noRemainderP m) [1..n]

printfn "%b" (checkNumber 10 2520)  // true
printfn "%b" (checkNumber 11 2520)  // false

checkNumber 10 2520
// tjekker: 2520 % 1 = 0 ?  ja
// tjekker: 2520 % 2 = 0 ?  ja
// tjekker: 2520 % 3 = 0 ?  ja
// ...
// tjekker: 2520 % 10 = 0 ? ja
// alle var true -> returnerer true

checkNumber 11 2520
// tjekker 1-10... alle true
// tjekker: 2520 % 11 = 0 ? nej
// én var false -> returnerer false

// -------------------------------------------------------------------------
// "Declare a tail–recursive F# function untilTrue f of type (int-> bool)-> int, 
// that repeatedly evaluates the function f on the integers n ≥ 1 until the 
// first n is found where f evaluates to true. The first n found is the result 
// of the evaluation. For instance, untilTrue (fun n-> n %3=0) evaluates to 3 
// because 1 % 3 is 1, 2 % 3 is 2 and 3 % 3 is 0."
// -------------------------------------------------------------------------
let untilTrue f =
    let rec loop n =
        if f n then n 
        else loop (n + 1)  // tail recursive - loop er det sidste der sker
    loop 1

printfn "%d" (untilTrue (fun n -> n % 4 = 0))  // 3


let untilTrue1 f =
    let rec loop n =
        printfn "tjekker %d" n
        if f n then n
        else loop (n + 1)
    loop 1

printfn "svar: %d" ( untilTrue1 (fun n -> n % 5 = 0 ))




let rec looping p = 
    if p >= 10 then p 
    else looping (p + p)

looping 5

// Hvorfor tail-recursive?
// loop (n + 1) er det allersidste der sker - ingen beregning efter kaldet.
// F# behøver ikke huske noget fra forrige kald og bruger ikke ekstra hukommelse.

// Hvad sker der med untilTrue (fun _ -> false)?
// Den kører for evigt - finder aldrig et n hvor f er true,
// så loop bliver ved med at kalde sig selv indtil programmet crasher.


// -------------------------------------------------------------------------
// "Declare an F# function findSmallest n of type int-> int, that evaluates 
// to the smallest number M, that can be evenly divided by the numbers 1,...,n.
// Hint: Can be implemented using untilTrue and checkNumber"
// -------------------------------------------------------------------------
let findSmallest n = untilTrue (checkNumber n)

printfn "%d" (findSmallest 3)   // 6
printfn "%d" (findSmallest 10)  // 2520