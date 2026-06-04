type Tree =
    Leaf
  | Node of Tree*int*Tree

let ex =
  Node(Node(Node(Leaf,2,Leaf),7,Leaf),
       9,
       Node(Node(Leaf,13,Leaf),21,Node(Leaf,25,Leaf)))

(* Error - Tree type is monomorphic and expects integer values *)
let ex =
  Node(Node(Node(Leaf,"2",Leaf),"7",Leaf),
       "9",
       Node(Node(Leaf,"13",Leaf),"21",Node(Leaf,"25",Leaf)))


let rec insert i = function
    Leaf                ->  Node(Leaf,i,Leaf)
  | Node(t1,j,t2) as tr ->
      match compare i j with
      | 0           -> tr
      | n when n<0  -> Node(insert i t1 , j, t2)
      | _           -> Node(t1,j, insert i t2)

let t1 = Node(Leaf, 3, Node(Leaf, 5, Leaf))
let t2 = insert 4 t1

let rec memberOf  i = function
    Leaf          -> false
  | Node(t1,j,t2) ->
    match compare i j with
      | 0   -> true
      | n when n<0 -> memberOf i t1
      | _          -> memberOf i t2

let rec inOrder = function
    Leaf          -> []
  | Node(t1,j,t2) -> inOrder t1 @ [j] @ inOrder t2

let ex =
  inOrder(Node(Node(Leaf,1,Leaf), 3, Node(Node(Leaf,4,Leaf), 5, Leaf)))

let rec delMin = function
  | Node(Leaf,i,t2) -> (i,t2)    
  | Node(t1,i,t2) -> let (m,t1') = delMin t1
                     (m, Node(t1',i,t2))

let rec delete j = function
    Leaf          -> Leaf
  | Node(t1,i,t2) ->
       match compare i j with
         n when n<0 -> Node(t1,i,delete j t2)
       | n when n>0 -> Node(delete j t1,i,t2)
       | _          ->
            match t2 with
              Leaf -> t1
            | _  -> let (m,t2') = delMin t2
                    Node(t1,m,t2')

let rec delMin = function
    Leaf -> (None, Leaf)
  | Node(Leaf,i,t2) -> (Some i,t2)
  | Node(t1,i,t2) -> match delMin t1 with
                         (None,t) -> (None,t)
                       | (Some m,t1') -> (Some m, Node(t1',i,t2))


type Tree<'a> =
      Leaf
    | Node of Tree<'a> * 'a * Tree<'a>


let rec inFoldBack f t e =
  match t with
      Leaf          -> e
    | Node(t1,x,t2) -> let er = inFoldBack f t2 e
                       inFoldBack f t1 (f x er)

let ta = Node(Node(Node(Leaf,-3,Leaf),0,Node(Leaf,2,Leaf)),5,Node(Leaf,7,Leaf))

let ex = inOrder ta
let ex = List.foldBack (-) (inOrder ta) 0
let ex = inFoldBack (-) ta 0
(* -3-(0-(2-(5-(7-0)))) *)
