// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 2: Values, operators, expressions and functions. 

let a = 42
let f x y = a + x + y
f 2

let even n = n % 2 = 0;; 

let isLowerCaseVowel ch =
    ch='a' || ch='e' || ch='i' || ch='o' || ch='u';;

let isLowerCaseConsonant ch =
    System.Char.IsLower ch && not (isLowerCaseVowel ch);;

isLowerCaseVowel 'i' && not (isLowerCaseConsonant 'i');;

isLowerCaseVowel 'I' || isLowerCaseConsonant 'I';;

not (isLowerCaseVowel 'z') && isLowerCaseConsonant 'z';;     

let nameAge(name,age) =
    name + " is " + (string age) + " years old";;
    
let adjString s = if even(String.length s)
                  then s else " " + s;;
                  
let square x = x * x;;

let rec power = function
    | (_, 0) -> 1.0                (* 1 *)
    | (x, n) -> x * power(x,n-1)   (* 2 *);;

let rec power a =
  match a with
  | (_,0) -> 1.0
  | (x,n) -> x * power(x,n-1);;

let rec power(x,n) =
  match n with
  | 0 -> 1.0
  | n' -> x * power(x,n'-1);;
                  
let plusThree = (+) 3;;

let f = fun y -> y+3;;        // f(y) = y+3
let g = fun x -> x*x;;        // g(x) = x*x
let h = fun z -> z*3;;

((fun y -> y+3) << (fun x -> x*x)) 4;;

let weight ro = fun s -> ro * s ** 3.0;;

let waterWeight = weight 1000.0;;

let methanolWeight = weight 786.5;;

let weight1 ro s = ro * s ** 3.0;;

let (.||.) p q = (p || q) && not(p && q);;

let (~%%) x = 1.0 / x;;

let eqText x y =
    if x = y then "equal" else "not equal";;

let ordText x y = if x > y then "greater"
                  else if x = y then "equal"
                  else "less";;

let ordText1 x y = match compare x y with
                   | t when t > 0 -> "greater"
                   | 0            -> "equal"
                   | _            -> "less";;


ordText1 (2,3,("a",'d'))  (2,3,("a",'c'))

let ((x,_),(_,y,_)) = ((1,true),("a","b",false));;

let f x =
  match x with
    (a,b) when a = b -> a
  | (a,b) when a < b -> b
  | (a,b) when a > b -> b    
  | (a,b) -> a
  
let (.<|.) f a = f a
let (.|>.) a f = f a
let it = System.Math.Sin <| 2.0
let it = 2.0 |> System.Math.Sin
(<|)
(|>)
(<<)
