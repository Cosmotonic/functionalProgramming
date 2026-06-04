(* Slide 4 *)
let rec posList = function
  | []    -> []
  | x::xs -> (x > 0)::posList xs

posList [4;  -5; 6]

(* Slide 5 *)
let rec addElems = function
  |[] -> []
  | (x,y)::zs -> (x+y)::addElems zs

addElems [(1,2) ;(3,4)]

(* Slide 6 *)
let rec map f = function
  | []    -> []
  | x::xs -> f x :: map f xs;;
let posList = map (fun x -> x > 0)
let addElems = map (fun (x,y) -> x+y)

let g xs = map (fun x -> x*x+1) xs

(* Slide 8 *)
let rec exists p = function
  | []    -> false
  | x::xs -> p x || exists p xs
exists (fun x -> x>=2) [1; 3; 1; 4]
  
let g xs = List.map (fun x -> x*x+1) xs

(* Slide 9 *)
let isMember x xs = List.exists (fun y -> y=x) xs

let isMember x = List.exists ((=) x)

(* Slide 10 *)
let rec forall p = function
  | []    -> true
  | x::xs -> p x && forall p xs
forall (fun x -> x>=2) [1; 3; 1; 4]

(* Slide 11 *)
let disjoint xs ys =
  List.forall (fun x -> not (List.exists (fun y -> y=x) ys)) xs
  
let disjoint xs ys = List.forall (fun x -> not (isMember x ys)) xs
  
let disjoint xs ys = List.forall (fun x -> (not >> isMember) x ys) xs

let disjoint xs ys = List.forall (fun y -> (not >> isMember) y xs) ys

let disjoint xs = List.forall (fun y -> (not >> isMember) y xs)

let isMember2 xs x = List.exists ((=) x) xs
let disjoint2 xs = List.forall (not >> (isMember2 xs)) xs

let subset xs ys =
  List.forall (fun x -> List.exists (fun y -> x=y) xs) ys

let subset xs ys = List.forall (fun y -> isMember y xs) ys  

let subset xs ys = List.forall (isMember2 xs) ys

(* Slide 12 *)
let rec filter p = function
  | []     -> []
  | x::xs -> if p x then x :: filter p xs
                    else filter p xs
filter System.Char.IsLetter ['1'; 'p'; 'F'; '-']

(* Slide 13 *)
let inter xs ys = List.filter (fun x -> isMember x ys) xs
let inter xs ys = List.filter (isMember xs) ys
let inter xs = List.filter (isMember xs)

(* Slide 14 *)
let rec tryFind p = function
     | x::xs when p x -> Some x
     | _::xs          -> tryFind p xs
     | _              -> None
tryFind (fun x -> x>3) [1;5;-2;8]

(* Slide 15 *)
let norm(x1:float,y1:float) = sqrt(x1*x1+y1*y1)
let rec sumOfNorms = function
           | []    -> 0.0
           | v::vs -> norm v + sumOfNorms vs
let vs = [(1.0,2.0); (2.0,1.0); (2.0, 5.5)]
sumOfNorms vs

(* Slide 16 *)
List.foldBack (+) [1; 2; 3] 0
List.foldBack (-) [1; 2; 3] 0

(* Slide 18 *)
let rec foldBack f xlst e =
  match xlst with
  | x::xs -> f x (foldBack f xs e)
  | [] ->e

let sumOfNorms vs = foldBack (fun v s -> norm v + s)  vs 0.0
let length xs = foldBack (fun _ n -> n+1) xs 0
let map f xs = foldBack (fun x rs -> f x :: rs) xs []

(* Slide 19 *)
let insert x ys = if isMember x ys then ys else x::ys

let union xs ys =
  List.foldBack (fun x rs -> insert x rs) xs ys

let union xs ys = List.foldBack insert xs ys
  
let union xs = List.foldBack insert xs
let union = List.foldBack insert (* Error - Value restriction *)

(* Slide 21 *)
let rec fold f e = function
  | x::xs -> fold f (f e x) xs
  | [] -> e

let rev xs = fold (fun rs x -> x::rs) [] xs  

let xs = [sin]

