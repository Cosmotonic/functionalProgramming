type Tree =
    Lf
  | Br of Tree*int*Tree

let rec insert i = function
    Lf                ->  Br(Lf,i,Lf)
  | Br(t1,j,t2) as tr ->
      match compare i j with
      | 0           -> tr
      | n when n<0  -> Br(insert i t1 , j, t2)
      | _           -> Br(t1,j, insert i t2)

let rec memberOf  i = function
    Lf          -> false
  | Br(t1,j,t2) -> match compare i j with
                   | 0   -> true
                   | n when n<0 -> memberOf i t1
                   | _          -> memberOf i t2

// Inorder toList
let rec toList = function
    Lf          -> []
  | Br(t1,j,t2) -> toList t1 @ [j] @ toList t2

let t = Br(Br(Lf,3,Lf), 1, Br(Lf, 0, Lf))
toList (insert 2 t)

memberOf 3 t

