// Exercise 1.1 
let sqrtExer x =  x * x // System.Math.Sqrt x
sqrtExer 1

// Exercise 1.2
let powerExr x n = System.Math.Pow(x, n) 
powerExr 2.0 2.0

// Exercise 1.3, book 1.1
let exer1 x = x + 4
exer1 3

// Exercise 1.4, book 1.2
let exer2 x y = System.Math.Sqrt(System.Math.Pow(x, 2.0) + System.Math.Pow(y, 2.0))
exer2 5.0 2.0


// Exercise 1.5, book 1.4 

let rec add = function 
    | 0 -> 0
    | n -> n + add(n-1)

add 4 // outputs 10 

// add(n) = 1 + 2 ... (n-1)+1
// add

// 3 + add(3-1)
// 3 + 2 + add(2-1) 
// 3 + 2 + 1 + add(1-1) // case catch 
// evaluates to 6 


// Exercise 1.6, book  1.5 book
let rec fibo = function 
    | 0 -> 0                        // Clause 1: F0 = 0
    | 1 -> 1                        // Clause 2: F1 = 1
    | n -> fibo(n-1) + fibo(n-2)    // Clause 3: Fn = Fn-1 + Fn-2
fibo 4

// Step  Expansion                               Result
// 1     fibo 4                                  fibo 3 + fibo 2
// 2     (fibo 2 + fibo 1) + (fibo 1 + fibo 0)   (fibo 2 + 1) + (1 + 0) // because 1 isnt running the recursive call anymore according to Pattern 2. 
// 3     ((fibo 1 + fibo 0) + 1) + 1             ((1 + 0) + 1) + 1
// 4     (1 + 0 + 1) + 1                         3


// Exercise 1.7, book  1.6 book
let rec sum = function 
| (m, 0) -> m
| (m, n) -> (m + n) + sum(m, (n - 1));;  

sum (10, 2)

// sum 10, 2  
// 10+2 + sum(10, 2-1)
// 12 + sum(10, 1) 
// 12 + 11 + sum(10, 1-1)     // clause 
// 12 + 11 + sum(10, 0)       // clause 
// 12 + 10 + 10 



// Exercise 1.8, book, 1.7 book
// Determine a type for each of the expressions:
// (System.Math.PI, fact -1)       // float * int                              // beskriv inputs
// fact(fact 4)                    // int                                      // beskriv output type
// power(System.Math.PI, fact 2)   // float                                    // beskriv output type
// (power, fact)                   // ((float * int) -> float) * (int -> int)  // beskriv func typer


// Exercise 1.9, book 1.8 
// environment 
let a = 5;;
let f a = a + 1;;
let g b = (f b) + a;;

env = [ a ↦ 5, 
        f ↦ <A function that takes a and returns a + 1>, 
        g ↦ <A funciton that takes b and retursn (f b) + a> ]

// f 3
// 3 + 1    (adds 3 to 1 )
// 4        (Resultat)
// 
// g 3 
// 4 + 5   ( calculates 3 + 1 and adds global a (5))
// 9       ( total sum )

// book nots  
// book nots  with stepwise evaluation
// book nots  

fun r -> System.Math.PI * r * r;;
it 2.0;;


let price = 125;;
price * 20;;
it / price = 20;;
System.Math.PI;;


(* Area of circle with radius r *)
let circleArea r = System.Math.PI * r * r;;
circleArea 1.0;;
2*3 + 4;;

function
| 1 -> 31 // January
| 2 -> 28 // February
| 3 -> 31 // March
| 4 -> 30 // April
| 5 -> 31 // May
| 6 -> 30 // June
| 7 -> 31 // July
| 8 -> 31 // August
| 9 -> 30 // September
| 10 -> 31 // October
| 11 -> 30 // November
| 12 -> 31;;// December

function
| 2 -> 28 // February
| 4 -> 30 // April
| 6 -> 30 // June
| 9 -> 30 // September
| 11 -> 30 // November
| _ -> 31;;// All other months

function
| 2 -> 28 // February
| 4|6|9|11 -> 30 // April, June, September, November
| _ -> 31 // All other months
;;



let daysOfMonth = function
| 2 -> 28 // February
| 4|6|9|11 -> 30 // April, June, September, November
| _ -> 31 // All other months
;;

daysOfMonth 2


// recursion for factorial 
let rec fact = function
| 0 -> 1
| n -> n * fact(n-1);;



fact 4

fact 4
4 * fact(4-1)                       (1)
4 * fact 3                          (2)
4 * (3 * fact(3-1))                 (3)
4 * (3 * fact 2)                    (4)
4 * (3 * (2 * fact(2-1)))           (5)
4 * (3 * (2 * fact 1))              (6)
4 * (3 * (2 * (1 * fact(1-1))))     (7)
4 * (3 * (2 * (1 * fact 0)))        (8) // keep in mind this step does no longer run the function again. The pattern doesnt call fact
4 * (3 * (2 * (1 * 1)))                     (9)
4 * (3 * (2 * 1))                           (10)
4 * (3 * 2)                                 (11)
4 * 6                                       (12)
24                                          (13)


let rec power = function
    | (x,0) -> 1.0                      // Clause 1: Enhver base i 0. potens er 1.0
    | (x,n) -> x * power(x,n-1)   // Clause 2: x^n = x * x^(n-1)

let rec power = match a with 
                    | (_,0) -> 1.0
                    | (x,n) -> x * power(x,n-1)


power(4.0, 2)

// my try 
// 4.0 * power ( 4.0, 2-1)
// 4.0 * power ( 4.0, 1)
// 4.0 * 4.0 * power (4.0, 1-1)
// 4.0 * 4.0 * power (4.0, 0)
// 4.0 * 4.0 * 1 

// gemini 
//   4.0 * power(4.0, 2-1)             (Clause 2, x is 4.0, n is 2)
//   4.0 * power(4.0, 1)               (Evaluering af 2-1)
//   4.0 * (4.0 * power(4.0, 1-1))     (Clause 2, x is 4.0, n is 1)
//   4.0 * (4.0 * power(4.0, 0))       (Evaluering af 1-1)
//   4.0 * (4.0 * 1.0)                 (Clause 1, x is 4.0)
//   4.0 * 4.0                         (Evaluering af parentes)
//   16.0                              (Resultat)


let rec gcd = function
| (0,n) -> n
| (m,n) -> gcd(n % m,m);; // remember % is modulo and gives the remainder

gcd(36, 116)
// gcd(12, 18)
// ⇝ gcd(18 % 12, 12)    (Clause 2: m=12, n=18. 18 % 12 er 6)
// ⇝ gcd(6, 12)          (Evaluering af modulo)
// ⇝ gcd(12 % 6, 6)      (Clause 2: m=6, n=12. 12 % 6 er 0)
// ⇝ gcd(0, 6)           (Evaluering af modulo)
// ⇝ 6                   (Clause 1: m=0, resultatet er n)

| (pattern) -> expression 


open System;;
[<EntryPoint>]

let main(param: string[]) =
printf "Hello %s\n" param.[0]
0;;'



( + ) 3 + 5 