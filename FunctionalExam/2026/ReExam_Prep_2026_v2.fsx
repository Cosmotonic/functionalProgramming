


// 1.1 
let rec take ((n:int),(xs:'a list)) : 'a list = 
    match xs with 
    | [] -> raise( System.Exception "Take: not enough elements")
    | x::rest -> if n > 0 then x::take ((n-1), rest) 
                 else rest

take (3, [1;2;4;5])

let partition (p:'a-> bool) (xs:'a list) : 'a list * 'a list = 
    let rec aux acc p xs = 
        match xs with 
        | [] -> acc 
        | x::rest ->
                let (higher,lower) = acc 
                if (p x) then aux (x::higher,lower) p rest else aux (higher,x::lower) p rest    
    aux ([],[]) p xs 

partition ((<)5) [1;2;5;3;5;9;9;10] 


let rec isSorted (xs:'a list) : bool when 'a : comparison = 
    match xs with 
    | [] -> true 
    | x::y::rest -> if x <= y then isSorted (y::rest) else false 
    | x -> true 

isSorted  [1;2;5;3;5;9;9;10] 
isSorted  [1;2;5;] 


// 1.2 
let rec tri n =
    if n = 0
        then 0
        else if n = 1 || n = 2
            then 1
            else tri (n - 1) +
                    tri (n - 2) +
                    tri (n - 3)

let natSeq = Seq.initInfinite (fun n -> n ) 
Seq.take 4 natSeq 

let triSeq = Seq.initInfinite (tri) 
Seq.take 4 triSeq
let triSeq' = Seq.cache triSeq  


// 2 


