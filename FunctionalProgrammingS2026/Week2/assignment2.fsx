// val dub : string -> string;; 

// 2.1 Dub string. 
let dub x = 
    x + x
dub "hi "


// 2.2 
let rec dupn = function 
    | (s, 1) -> s
    | (s, n) -> (s) + dupn( s, ( n-1))

dupn ("hi ", 3);; 


// 2.3 
let timediff (t1x, t1y) ( t2x, t2y) =
     ((t2x * 60) + t2y) - ((t1x * 60) + t1y )

timediff(12, 34) (11, 35)
timediff(12,34) (13,35)



// 2.4
let minutes (h, m) = 
    timediff(00,00) (h,m)
minutes(14,24)


// 2.5 
let rec power = function 
                    | (s,1) -> s
                    | (s,n) -> s + power(s,n-1)
power("hello ", 2)


// 2.6  ( Book 2.8 )

let rec bino = function
    | (_, 0) -> 1                               // if its 0 column it will always be a 1. 
    | (n, k) when n = k -> 1         // According to rule nr 2 k must not be bigger than n. 
    | (n, k) -> bino(n-1, k-1) + bino(n-1, k)

bino(4, 2) 

// Unpacking each level 
// binom(4, 2)
// (binom(3, 1) + binom(3, 2))
// ((binom(2, 0) + binom(2, 1)) + (binom(2, 1) + binom(2, 2)))
// ((1 + (binom(1, 0) + binom(1, 1))) + ((binom(1, 0) + binom(1, 1)) + 1))
// ((1 + (1 + 1)) + ((1 + 1) + 1))
// (1 + 2) + (2 + 1)
// 3 + 3
// 6


// 2.7 ( Book 2.9 )
let rec f = function
| (0,y) -> y
| (x,y) -> f(x-1, x*y);;


// 1. Int

// 2. x=0

// 3. 
// (2,3)
// (2-1, 2*3)
// (1, 6) 
// (1-1, 2*6) 
// ( 0, 12) 

// 4. Its factorial. The result always multiply the next value. 


