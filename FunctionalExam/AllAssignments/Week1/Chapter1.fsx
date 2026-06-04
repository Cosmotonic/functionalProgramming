// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 1: Getting started. 

2*30+4+2*3

let price = 25*5
let newPrice = 2*price
let newPrice = 34
newPrice > 500

let circleArea r = System.Math.PI * r * r
circleArea 1.0
circleArea(3.2)

fun r -> System.Math.PI * r * r

fun b ->
  match b with
  | true -> 1
  | false -> 0 

fun i ->
  match i with
  | 0 -> true
  | 1 -> false
    
fun i ->
  match i with
  | 0 -> true
  | _ -> false


function
  | 0 -> true
  | _ -> false

function
  | 1 -> 31 // January
  | 2 -> 28 // February // not a leap year
  | 3 -> 31 // March
  | 4 -> 30 // April
  | 5 -> 31 // May
  | 6 -> 30 // June
  | 7 -> 31 // July
  | 8 -> 31 // August
  | 9 -> 30 // September
  | 10 -> 31 // October
  | 12 -> 31 // December

function
  | 2 -> 28  // February
  | 4 ->30   //April
  | 6 ->30   //June
  | 9 -> 30  // September
  | 11 -> 30 // November
  | _ -> 31  // All other months

function 
  | 2 -> 28 // February
  | 4|6|9|11 -> 30 // April, June, September, November
  | _ -> 31 // All other months

let daysOfMonth =
  (fun x ->
       match x with
       | 1 -> 31 // January
       | 2 -> 28 // February // not a leap year
       | 3 -> 31 // March
       | 4 -> 30 // Aprila
       | 5 -> 31 // May
       | 6 -> 30 // June
       | 7 -> 31 // July
       | 8 -> 31 // August
       | 9 -> 30 // September
       | 10 -> 31 // October
       | 11 -> 30 // November
       | 12 -> 31) // December


let daysOfMonth = function
    | 2        -> 28  // February
    | 4|6|9|11 -> 30  // April, June, September, November
    | _        -> 31  // All other months
;;  

let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)

//fact(0) = 1
//fact(n) = n * fact(n - 1) [if n > 0]
let rec fact2 n =
  match n with
    | 0 -> 1
    | _ -> n * fact2(n-1)

let rec power = function
  | (_,0) -> 1.0
  | (x,n) -> x * power(x,n-1)

let rec fact n = if n = 0 then 1 else n * fact(n-1)
let rec power (x,n) = if n=0 then 1.0 else x * power(x,n-1)

(* Tupples*)
(42,5)

(34.0,"A string")

("and why not a function", fact , power)

let rec gcd = function
    | (0,n) -> n
    | (m,n) -> gcd(n % m,m)

let l1 = [2;3;6]

let l2 = ["a";"ab";"abc";""]

let l3 = [System.Math.Sin;System.Math.Cos]

let l4 = [(1,true); (3,true)]

let l5 = [[]; [1]; [1;2]]

let rec suml = function
   [] -> 0
 | x::xs -> x + suml xs

let r1 = suml [1;2]

let rec (<=.) xs ys = 
  match (xs,ys) with
    ([],_) -> true
  | (_,[]) -> false
  | (x::xs',y::ys') -> x<=y && xs' <=. ys'

let r2 = [1;2;3] <=. [1;2]
let r3 = [1;2] <=. [1;2;3]




