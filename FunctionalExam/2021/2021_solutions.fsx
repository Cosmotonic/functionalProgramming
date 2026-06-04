
// ALWAYS read full section and write number and comment into .fsx 
// Divide and conquere the problem in steps. 

// The weight of a non–leaf node is the sum of the weights of all the leaf nodes its left subtree.

type Rope =
    Leaf of string * int
    | Node of Rope * int * Rope

let rope1 = Node(Node(Leaf("I_lik",5),5,Leaf("e_functional_pr",15)), 
                    20,
                    Node(Leaf("ogr",3),3,Leaf("amming",6))) // val rope1: Rope = Node (Node (Leaf ("I_lik", 5), 5, Leaf ("e_functional_pr", 15)), 20, Node (Leaf ("ogr", 3), 3, Leaf ("amming", 6)))

// 1.1.1 
let rope2 = Node(Leaf("_and", 4), 
            4, 
            Node(Leaf("_very_", 6), 6, Leaf("Much_F#", 7))) // val rope2: Rope =  Node (Leaf ("_and", 4), 4, Node (Leaf ("_very_", 6), 6, Leaf ("Much_F#", 7)))

// 1.1.2 
// Monomorphic 

// 1.1.3 
let rope3 = Node(Node(Leaf("Example_", 8), 8, Leaf("With_", 5)), 13, Leaf("5_nodes", 7 )) // val rope3: Rope = Node (Node (Leaf ("Example_", 8), 8, Leaf ("With_", 5)), 13,Leaf ("5_nodes", 7))
 
// 1.2.1 Get lenght of all strings in rope (legth is on leafs)
let rec length (r:Rope) : int =    
    let rec aux r acc = 
        match r with 
        | Leaf (_, v) -> acc + v 
        | Node (_, v, r) -> (acc + v) + (aux r acc) 
    aux r 0 
length rope1 // val it: int = 29
length rope2 // val it: int = 17
length rope3 // val it: int = 20

// 1.2.2. flatten - concat all 
let rec flatten (r:Rope) : string = 
    let rec aux r acc = 
        match r with 
        | Leaf (s, v) -> acc + s
        | Node (l, v, r) -> (aux l acc) + (aux r acc) 
    aux r ""
flatten rope1

// 1.2.3. maxDepth - get the depth of the tree (incldue root) (compare recursively) get bigger 
let maxDepth (r:Rope) : int = 
    let depth = 0 
    let rec aux r acc =  
        match r with 
        | Leaf (_,_) -> acc + 1 
        | Node (l,_,r) -> 
            let rightDepth = ( acc + 1 ) + (aux r acc) 
            let leftDepth = ( acc + 1 ) + (aux l acc) 
            if leftDepth > rightDepth then leftDepth
            else rightDepth
    aux r 0 
maxDepth rope1 // val it: int = 3 
maxDepth rope2 // val it: int = 3
maxDepth rope3 // val it: int = 3

// 1.2.4. Index - get the index letter on a rope leaf - husk der kun er tekst i leaf's 
let index (i:int) (r:Rope) : char = 
    let ropeLength = length r 
    let RopeFlatten = flatten r 
    if i > ropeLength then raise (System.Exception ("Index out of string bounds"))
        else RopeFlatten.Substring(i,1)[0]
    // alternative: (flatten r).Substring(i,1)[0]

index 5 rope1 // val it: char = 'e'  


// 1.3.1 parent two trees. 
let concat (r1:Rope) (r2:Rope) : Rope = 
    Node(r1,  length r2,  r2) // val it: Rope = Node (Node(Node (Leaf ("I_lik", 5), 5, Leaf ("e_functional_pr", 15)), 20, Node (Leaf ("ogr", 3), 3, Leaf ("amming", 6))), 17, Node (Leaf ("_and", 4), 4, Node (Leaf ("_very_", 6), 6, Leaf ("Much_F#", 7))))
concat rope1 rope2

// 1.3.2 Pretty print   
let prettyPrint (r:Rope) : string = 
    let output = flatten r
    printfn "--> %s" output
    output
prettyPrint (concat rope1 rope2)  // val it: string = "I_like_functional_programming_and_very_Much_F#"


// question 2. 

// 2.1.1 
// They are Chars because they are defined with '' 
type Bucket<'a> = {
    sizeBucket : int; 
    elems : List<'a>
}
type UList<'a>  = Bucket<'a> List 

// 2.1.2
let ulist01 = [ { sizeBucket = 4; elems = ['A'; 'B'; 'C'; 'D'] };
                { sizeBucket = 2; elems = ['E'; 'F']};
                { sizeBucket = 6; elems = ['G'; 'H'; 'I'; 'J'; 'K'; 'L'] };
                { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
                { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
                { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]
let ulist02 = [ { sizeBucket = 4; elems = ['A'; 'B'; 'C'; 'D'] };
                { sizeBucket = 4; elems = ['E'; 'F'; 'K'; 'L'] };
                { sizeBucket = 4; elems = ['G'; 'H'; 'I'; 'J'] };
                { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
                { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
                { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]

// 2.1.3
ulist01 = ulist02 // val it: bool = false 
// because both size of the bucket, value of chars and lenght of elems array has to match. 

// 2.2.4 
let emptyUL() : UList<'a> = 
    [{ sizeBucket = 0; elems = [] }];
let eBuck = emptyUL

// 2.2.1 Size - so all sizebuckets accumulated. 
let sizeUL (ul:Bucket<'a> List) : int = 
    let folder = List.fold (fun acc e -> e.sizeBucket + acc ) 0 ul 
    folder 

// 2.2.2 isEmptyUl - bool 
let isEmptyUL (ul:Bucket<'a> List) : bool = 
    if sizeUL ul = 0 then true else false 
isEmptyUL (emptyUL())

// 2.2.3 existsUL - if char 'A' exists in bucket true 
let rec existsUL (e:'a) (ul:Bucket<'a> List) : bool = 
    match ul with
    | [] -> false 
    | x::tail -> if (List.contains e x.elems) then true else existsUL e tail  

existsUL 'A' (emptyUL()) // val it: bool = false
existsUL 'A' ulist01     // val it: bool = true

// 2.2.4. itemUL - gets an index. Return the char at the index. 
let itemUL (ul:Bucket<'a> List) (i:int ) = 
    if i > ( sizeUL ul ) then failwith "fail" else 
        let elemList = List.rev ( List.fold ( fun acc e -> ( List.rev e.elems) @ acc) [] ul)
        elemList[i]
let output = itemUL ulist01 5


// 2.2.5. filter - filters out all on predicate. Eg if the element is is smaller than 'I' make a list of everything before I. 
let filterUl (p:'a->bool) (ul:Bucket<'a>list):Bucket<'a>list=
    let rec aux ul acc=
        match ul with
        |[]-> List.rev acc
        |x::tail-> // x= {size, elems[]} tail = {},{},{},{}
            let filteredList= List.filter p x.elems
            let newBucket= {sizeBucket = filteredList.Length; elems=filteredList}
            if filteredList.IsEmpty then aux tail acc // if empty skip.
            else aux tail (newBucket::acc)      // else add to acc. 
    aux ul []
    
filterUl (fun e -> e < 'I') ulist01

// 2.3. - RECORDS record, rec, 
// 1. The maximum allowed number of elements in a bucket, maxSizeBucket, is 4.
// 2. Wewill not allow empty buckets in our representation.
// 3. The size of a bucket, sizeBucket, must be equal to the number of elements in the bucket (elems).
let ulist03Wrong = [ { sizeBucket = 2; elems = [1] };
                    { sizeBucket = 0; elems = [] };
                    { sizeBucket = 5; elems = [2;3;4;5;6] } ]
let ulist03Wrong2 = [ { sizeBucket = 1; elems = [1] };
                    { sizeBucket = 0; elems = [] };
                    { sizeBucket = 5; elems = [2;3;4;5;6] } ]

// 2.3.1 chk - apply the three rules above. return true if non fails and false if any of them do. 
let chkUL (ul:Bucket<'a> List) : bool = 
    if ul.Length > 4 then false 
    else if ul.IsEmpty then false 
    else 
        let rec aux ul =
            match ul with
            |[]-> true // all elements were not false, so it must be true. 
            |x::tail-> if x.elems.Length = x.sizeBucket then aux tail else false 
        aux ul 
chkUL ulist03Wrong  // val it: bool = false 
chkUL ulist03Wrong2 // val it: bool = true

// 2.3.2 map - apply a function to all elements in all lists
let map (f:'a->'b) (ul:Bucket<'a> List) : Bucket<'b> List = 
    let rec aux ul acc =
        match ul with
        |[]-> List.rev acc
        |x::tail-> // x= {size, elems[]} tail = {},{},{},{} // same as filter func above
            let mappedList= List.map f x.elems // .map changes all elements in a list. 
            let newBucket= {sizeBucket = mappedList.Length; elems = mappedList}
            aux tail (newBucket::acc) 
    aux ul []

map (int) ulist01


// 2.3.3 fold - fold everythign into one long string. 
let fold f (a:'a) (ul:Bucket<'b> List) = 
    let elemList = List.rev ( List.fold ( fun acc e -> ( List.rev e.elems) @ acc) [] ul)
    List.fold f a elemList
    
    // ul |> List.fold ( fun acc e -> ( List.rev e.elems) @ acc) [] | List.rev |> List.fold f a 
    
    // ul 
    // |> List.collect(fun elem -> elem.elems) 
    // |> List.fold f 

fold (fun a c-> a+((string)c)) "" ulist01 


// 3. Question 
// 3.1.1. 
let rec G ((m:int), (n:int)) : int = 
    if n <= 0 then m+n 
    else G ((2*m),(n-1))+m
G (10,10)

// 3.1.2 
// It is not recursive because the last thing we do is not call the fucntion it is plussing m 

// 3.1.3 - SEQUENCES / sequence tag seq. 
let rec GA ((m:int), (n:int)) acc  : int = 
    if n <= 0 then m+n + acc  // we add the accumulator in the end for tail recursion here. 
    else GA ((2*m), (n-1)) (m+acc) 
GA (10,10) 0 

// 3.2.1 - 
let mySeq : seq<int*int> = 
    seq {
        for i in 1..100 do 
            for j in 1..100 do 
                yield (i,j)
        }

Seq.take 4 mySeq 

// 3.2.2 
let gSeq : seq<int> = 
    Seq.map G mySeq 
gSeq // G (1,1) 0 
     // G (2,0) (1+0)
     // base case: 2+0+1


// Question 4. 
type inst =
    ADD
    | SUB
    | PUSH of int
    | LABEL of string
    | IFNZGOTO of string
    | EXIT

type stack = inst list

let insts01 =
    [PUSH 10;
    PUSH 12;
    ADD;
    EXIT]

let insts02 =
    [PUSH 10;
    LABEL "sub1";
    PUSH 1;
    SUB;
    IFNZGOTO "sub1";
    EXIT]

// 4.1. 
let execInsts (inst:inst list) : int = 
    let rec exec inst s =
        match (inst,s) with                                    // s is stack, is instructions, 
        | (ADD::ins,v1::v2::s)-> exec ins (v1+v2::s)
        | (SUB::ins,v1::v2::s)-> exec ins (v2-v1::s)
        | (PUSH i::ins,s) ->  exec ins (i::s)
        | (EXIT ::ins,v1::s) -> v1                             // output the 
        | (LABEL lab::_,s)-> failwith "LABEL not implemented"
        | (IFNZGOTO lab::_,s)-> failwith "IFNZGOTO not implemented"
        | _-> failwith "Missing stack values for instruction" // incase of PUSH 10 and instructing ADD

    exec inst []
execInsts insts01

// 4.2.1
let buildEnv (insts:inst list) : Map<string,int> = 
    let rec build idx (env: Map<string, int>) insts = 
        match insts with 
        []-> env // once we are out of elements we return the env. 
        | LABEL lab :: rest-> build idx (env.Add (lab,idx)) rest // rest is remainign inststructions
        | _ :: rest-> build (idx+1) env rest 
    build 0 Map.empty insts

buildEnv insts02

// 4.2.2 
type resolvedInst =
    RADD
    | RSUB
    | RPUSH of int
    | RIFNZGOTO of int
    | REXIT

type env = Map<string,int>
let lookup l m =
    match Map.tryFind l m with
    None-> failwith "Value not in map"
    | Some v-> v

let resolveInsts (insts: inst list) (env:Map<string, int>) : Map<int,resolvedInst> =
    let rec resolve idx insts= 
        match insts with 
        []-> Map.empty
        | LABEL lab :: insts-> resolve idx insts
        | ADD :: insts->     Map.add idx RADD (resolve (idx+1) insts)
        | IFNZGOTO lab :: insts-> Map.add idx (RIFNZGOTO (lookup lab env)) (resolve (idx+1) insts)
        | SUB :: insts ->    Map.add idx RSUB (resolve (idx+1) insts)
        | PUSH v :: insts -> Map.add idx (RPUSH v) (resolve (idx+1) insts)
        | EXIT :: insts->    Map.add idx REXIT (resolve (idx+1) insts) 
    resolve 0 insts 

resolveInsts insts02 (buildEnv insts02)