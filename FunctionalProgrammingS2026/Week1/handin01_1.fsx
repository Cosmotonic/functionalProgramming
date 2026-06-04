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
// (power, fact)                   // (float * int -> float) * (int -> int)  // beskriv func typer


// Exercise 1.9, book 1.8 
// environment 
let a = 5;;
let f a = a + 1;;
let g b = (f b) + a;;

env = [ a ↦ 5, 
        f ↦ <A function that takes a and returns a + 1>, 
        g ↦ <A funciton that takes b and retursn (f b) + a> ]
