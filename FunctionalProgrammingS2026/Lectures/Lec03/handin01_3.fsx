// 3.1 
// 2.10 Consider the following declaration:
// let test(c,e) = if c then e else 0;;


// 1. What is the type of test?
    // boolean * int -> int 
// 2. What is the result of evaluating test(false, fact(-1))? 
    // The result would be an error because the fact(-1) is an infinity loop that never produce an integer. 
// 3. Compare this with the result of evaluating // it terminates because the fact method never ends looping. 
    // if false then fact -1 else 0
    // The result of the let is not depending on the inputs. False is hardcoded, due to lazy evaluatoin of f# it only reach 0. 
    // it never runs into the infinity loop. 

let rec fact = function
| 0 -> 1
| n -> n * fact(n-1);;


// let test(c,e) = if true then fact -1 else 10;;
// test(false, 2)
let testtwo(c,e) = if false then fact -1 else 0;;
testtwo(true, 1)


// 3.2 - 2.13
// curry f is the function g where g x is the function h where h y = f(x, y).
// curry : (’a * ’b -> ’c) -> ’a -> ’b -> ’c

let rec bino = function
    | (_, 0) -> 1                               // if its 0 column it will always be a 1. 
    | (n, k) when n = k -> 1         // According to rule nr 2 k must not be bigger than n. 
    | (n, k) -> bino(n-1, k-1) + bino(n-1, k);;

let rec cnt = function
    | 0 -> 0                             // if its 0 column it will always be a 1. 
    | (n) -> n + (n-1);;
cnt 2

let inty x =  x + x
let intx y =  y * y 
// let curry f = fun x -> fun y -> f(x, y)

//  f(x, y)        <- 4. til sidst kalder vi den originale funktion med begge
//  fun y ->       <- 3. som returnerer en funktion der tager y
//  fun x ->       <- 2. der returnerer en funktion der tager x
//  curry f        <- 1. vi starter med at give curry en funktion f

// Så i stedet for at tænke "vi kalder den bagfra", så tænk:

// curry f    ->    fun x    ->    fun y    ->    f(x,y)
// giv f            giv x          giv y          få svar

// ('a * 'b -> 'c)    <- f  (en funktion der tager et par)
// 'a                 <- x  (første argument)
// 'b                 <- y  (andet argument)
// 'c                 <- f(x, y)  (svaret)

// let beregnPris(rabat, pris) = 
//     pris * (1.0 - rabat)
// let curry f = fun x -> fun y -> f(x, y)
// let blackFriday = curry beregnPris 0.20

// printfn "%f" (blackFriday 100.0)  // = 80.0
// printfn "%f" (blackFriday 250.0)  // = 200.0
// printfn "%f" (blackFriday 399.0)  // = 319.2

//----- UNCURRY FUNCTION USING CURRY TO TAKE ARGUMENTS ------//
let beregnPrisMatesRate(mateRate, rabat, pris) =  mateRate * pris * (1.0 - rabat)  // laver min formel
let curry beregnPrisMatesRate = fun x -> fun y -> fun z -> beregnPrisMatesRate(x,y,z) // laver stepwise rækkefølge på beregnPrisMatesRate
let setMatesRate = curry beregnPrisMatesRate 10.0 // tager formlen og smider første værdi ind på x
let beregnRabat  = setMatesRate  0.150 // samler op og smider næste værdi ind på y

printfn "%f" (beregnRabat 100.0)  // printer resultatet basseret på z input af 100
printfn "%f" (beregnRabat 150.0)  // printer resultatet basseret på z input af 100
printfn "%f" (beregnRabat 130.0)  // printer resultatet basseret på z input af 100

let beregnNoRabatPris  = setMatesRate  0.100 // samler op og smider næste værdi ind på y
printfn "%f" (beregnNoRabatPris 130.0)  // printer resultatet basseret på z input af 100


//----- UNCURRY FUNCTION USING CURRY TO TAKE ARGUMENTS ------//
// Denne er allerede curried fra starten - ingen curry funktion brugt
let beregnnYPris rabat pris = pris * (1.0 - rabat)
let uncurry g = fun (x, y) -> g x y
let parVersion = uncurry beregnnYPris

// Nu kan du kalde den med et par 
printfn "%f" (parVersion(0.20, 100.0))  // = 80.0


let beregnNyPris(rabat, pris) = pris * (1.0 - rabat)
let curry f = fun x -> fun y -> f(x, y)
let curryVersion = curry beregnNyPris

// curried - ét ad gangen

printfn "%f" (curryVersion 0.20 100.0)  // = 80.0
beregnNyPris(0.20, 100.0)   // uncurried - tuple
curryVersion 0.20 100.0     // curried   - ét ad gangen


// Exercise 3.3 Write a function
// downTo:int->int list
// so that downTo n returns the n-element list [n; n-1; ...; 1]. You must use if-then-else expressions to
// define the function.
// Secondly define the function downTo2 having same semantics as downTo. This time you must use pattern
// matching.

// 
let rec downTo_condition n = 
    if n = 0 then []
    else n :: downTo_condition (n-1)

downTo_condition 4

let rec downTo_pattern = function 
    0-> []
    | n-> n :: downTo_pattern (n-1)

downTo_pattern 4


// Exercise 3.4 Write a function
// removeOddIdx:int list->int list
// so that removeOddIdx xs removes the odd-indexed elements from the list xs:
// removeOddIdx [x0; pound; shilling; pence; x4; ...] = [x0; shilling; x4; ...]
// removeOddIdx [] = []
// removeOddIdx [x0] = [x0]

let rec removeOddIdx = function
    | [] -> [] // "Er listen tom? Så er der ingen flere elementer at tilføje, returnér ingenting."
    | [x] -> printfn "svar: %d" x; [x] // "Er der præcis ét element tilbage, returnér det som en liste med ét element."
    | pound::shilling::xs-> pound :: removeOddIdx xs

removeOddIdx [0;1;2;3;4;5;6;7;8]

// Exercise 3.5 Write a function
// combinePair:int list->(int*int) list
// so that combinePair xs returns the list with elements from xs combined into pairs. If xs contains an odd
// number of elements, then the last element is thrown away:
// combinePair [pound; shilling; pence; x4] = [(pound,shilling);(pence,x4)]
// combinePair [pound; shilling; pence] = [(pound,shilling)]
// combinePair []   = []
// combinePair [pound] = []


let rec combinePair = function 
    | [] -> []
    | [x] -> []
    | pound::shilling::rest -> (pound,shilling) :: combinePair rest; 

combinePair [0;1;2;3;4;5;6;7;8]


// example from slides 
let rec unzip = function
    | []-> ([],[])
    | (x,y)::rest-> let (xs,ys) = unzip rest
                    (x::xs,y::ys);;
                    unzip [(1,"a");(2,"b")];;

// val it : int list * string list = ([1; 2], ["a"; "b"])


// Exercise 3.6 Solve HR, exercise 3.2. ( page 80)
// The former British currency had 12 pence to a shilling and 20 shillings to a pound. Declare
// functions to add and subtract two amounts, represented by triples of
// integers, and declare the functions when a representation by records is used. Declare the 
// functions in infix notation with proper precedences, and use patterns to obtain readable declarations.

type currency = {
        pounds : int; 
        shilling : int; 
        pence : int
    }


// (pounds, shillings, pence) 
// 5 pounds, 10 shilling, 5 pencen  -  3 pounds, 5 shilling, 2 pence

let makeCurrencyFromPence a  =
    let po = a / (20*12)                // 5 
    let pound_remainder = a % (20*12)   // 1250 % 240 = 50 
    let shill = pound_remainder / 12    // 50 / 12 = 2     
    let pe =   pound_remainder % 12     // 50 % 12 = 10 

    (po, shill, pe)    ///say we get 1250 pence

let converteToPence (pound,shilling,pence) = 
    let pounds_to_pence = pound * 20 * 12 
    let shilling_to_pence = shilling * 12 
    let total = pence + shilling_to_pence + pounds_to_pence 
    total

// Denne funktion skal hedde (.-.) når jeg har lavet logiken 
let (.-.) (pound,shilling,pence) (pound_1,shilling_1,pence_1) =

    let totalPence = converteToPence(pound, shilling, pence) - converteToPence (pound_1, shilling_1, pence_1)
    let toCurrency  = makeCurrencyFromPence totalPence
    toCurrency;;  

// Denne funktion skal hedde (.+.) når jeg har lavet logiken 
let (.+.) (pound,shilling,pence) (pound_1,shilling_1,pence_1) = 
    let totalPence = converteToPence(pound, shilling, pence) + converteToPence (pound_1, shilling_1, pence_1)
    let toCurrency  = makeCurrencyFromPence totalPence
    toCurrency;;   

( 10, 6, 6 ) .+. ( 10, 14, 6 )
( 12, 6, 6 ) .-. ( 10, 6, 6 )

   Dette er en 
   multiline kommentar 


// ---------------------------------------------
// Exercise 3.7 Solve HR, exercise 3.3.
// The set of complex numbers is the set of pairs of real numbers. 
// Complex numbers behave almost like real numbers 
// if addition and multiplication are defined by:

//                            (a, b) + (c, d) = (a + c, b + d)
//                            (a, b) · (c, d) = (ac − bd, bc + ad)

// 1. Declare suitable infix functions for addition and multiplication of complex numbers.

let (.+.) (a, b) (c, d) = 
    (a + c, b + d)

let (.*.) (a, b) (c, d) = 
    (a*c - b*d, b*c + a*d)

// 2. The inverse of (a, b) with regard to addition, that is, −(a, b), is (−a, −b), and the inverse of
// (a, b) with regard to multiplication, that is, 1/(a, b), is (a/(a^2 +b^2), −b/(a^2 +b^2)) (provided
// that a and b are not both zero). Declare infix functions for subtraction and division of complex numbers
// 3. Use let-expressions in the declaration of the division of complex numbers in order to avoid
// repeated evaluation of identical subexpressions.

let negativeComplex (a: int, b) = (-a, -b)

let invComplex (a, b) = 
    if a = 0 && b = 0 then 
        failwith "Cannot invert"
    else
        let denominator = a*a + b*b // a^2 + b^2 
        (a / denominator, -b / denominator)

let (.-.) (a, b) (c, d) = 
    (a, b) .+. (negativeComplex (c, d))

let (./.) (a, b) (c, d) = 
    (a, b) .*. (invComplex (c, d))

let (./.) (a, b) (c, d) = 
    if c = 0.0 && d = 0.0 then 
        failwith "Cannot divide by (0,0)"
    else
        let denom = c*c + d*d
        let invReal = c / denom
        let invImag = -d / denom
        let realPart = a * invReal - b * invImag
        let imagPart = b * invReal + a * invImag
        (realPart, imagPart)


let rec (<=.) xs ys =
            match (xs,ys) with
            | ([],_)-> true
            | (_,[])-> false
            | (x::xs',y::ys')-> x<=y && xs' <=. ys';;
            [1;2;3] <=. [1;2];;
[1;2;3] <=. [1;2;3];;



let rec altsum = function 
    | [] -> 0 
    | x1::xs -> x1 + (-1*altsum xs)

altsum[2; -1; 2]







let rec altsum = function 
    | [] -> 0
    | [x] -> x
    | x1::x2::xs -> x1 - x2 + altsum xs

altsum  [ 2; -1   ;   3; 3; ]
                [   3    ;     0    ]
                [ recur  ;   recur ]



 2 - (-1) + altsum xs 
 2 - (-1) + 


2 - (-1)  // leftover is 3
2 + 1 = 3 

altsum [3]

3 + 3 





















// Exercise 3.8 Solve HR, exercise 4.4