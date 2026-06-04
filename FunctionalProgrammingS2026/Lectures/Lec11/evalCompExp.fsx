(* Simple Evaluator *)
type expr =
  CstI of int
| Prim of string * expr * expr
  
let ex = Prim("+", CstI 7, Prim("*", CstI 9, CstI 10))

let opEval op v1 v2 : int =
  match op with
    "+" -> v1 + v2
  | "*" -> v1 * v2
  | "/" -> v1 / v2

let rec eval e : int =
  match e with
    CstI i -> i
  | Prim(op, e1, e2) ->
      let v1 = eval e1
      let v2 = eval e2
      opEval op v1 v2

eval ex

let opEvalOpt op v1 v2 : int option =
  match op with
    "+" -> Some (v1 + v2)
  | "*" -> Some (v1 * v2)
  | "/" -> if v2 = 0 then None else Some (v1 / v2)

let rec evalOpt e : int option =
  match e with
    CstI i -> Some i
  | Prim(op, e1, e2) ->
    match evalOpt e1 with
      None -> None
    | Some v1 ->
        match evalOpt e2 with
          None -> None
        | Some v2 -> opEvalOpt op v1 v2

evalOpt ex
    
let opEvalSet op v1 v2 : int Set =
  match op with
    "+" -> Set [v1 + v2]
  | "*" -> Set [v1 * v2]
  | "/" -> if v2 = 0 then Set.empty else Set [v1 / v2]
  | "choose" -> Set [v1;v2]

let rec evalSet e : int Set =
  match e with
    CstI i -> Set [i]
  | Prim(op, e1, e2) ->
    let s1 = evalSet e1
    let yss =
      Set.map (fun v1 ->
                 let s2 = evalSet e2
                 let xss = Set.map (fun v2->opEvalSet op v1 v2) s2
                 Set.unionMany xss) s1
    Set.unionMany yss

let exSet = Prim("+", CstI 7, Prim("*",
                                   Prim("choose",CstI 9, CstI 10),
                                   Prim("choose",CstI 2, CstI 23)))
evalSet exSet

type 'a trace = string list * 'a

let opEvalTrace op v1 v2 : int trace =
  match op with
    "+" -> (["+"],v1 + v2)
  | "*" -> (["*"],v1 * v2)
  | "/" -> (["/"],v1 / v2)

let rec evalTrace e : int trace =
  match e with
    CstI i -> ([],i)
  | Prim(op, e1, e2) ->
      let (trace1, v1) = evalTrace e1
      let (trace2, v2) = evalTrace e2
      let (trace3, res) = opEvalTrace op v1 v2
      (trace1@trace2@trace3, res)

evalTrace ex

type OptionBuilder() =
  member this.Bind(x,f) =
    match x with
        None -> None
      | Some v -> f v
  member this.Return x = Some x
  member this.ReturnFrom x = x

let optionM = OptionBuilder()

let rec evalOptM e : int option =
  match e with
    CstI i -> optionM { return i }
  | Prim(op, e1, e2) ->  
      optionM { let! v1 = evalOptM e1
                let! v2 = evalOptM e2
                return! opEvalOpt op v1 v2 }

evalOptM ex

type SetBuilder() =
  member this.Bind(x,f) = Set.unionMany(Set.map f x)
  member this.Return x = Set [x]
  member this.ReturnFrom x = x

let setM = SetBuilder()

let rec evalSetM e : int Set =
  match e with
    CstI i -> setM { return i }
  | Prim(op, e1, e2) ->
    setM { let! v1 = evalSetM e1
           let! v2 = evalSetM e2
           return! opEvalSet op v1 v2 }

evalSetM exSet

type TraceBuilder() =
  member this.Bind(x,f) =
    let (trace1, v) = x
    let (trace2, res) = f v
    (trace1 @ trace2, res)
  member this.Return x = ([], x)
  member this.ReturnFrom x = x

let traceM = TraceBuilder()

let rec evalTraceM e : int trace =
  match e with
    CstI i -> traceM { return i }
  | Prim(op, e1, e2) ->
      traceM { let! v1 = evalTraceM e1
               let! v2 = evalTraceM e2
               return! opEvalTrace op v1 v2 }

evalTraceM ex

type IdentityBuilder() =
  member this.Bind(x,f) = f x
  member this.Return x = x
  member this.ReturnFrom x = x

let identM = new IdentityBuilder()

let rec evalIdentityM e : int =
  match e with
    CstI i -> identM { return i }
  | Prim(op, e1, e2) ->
      identM { let! v1 = evalIdentityM e1
               let! v2 = evalIdentityM e2
               return! opEval op v1 v2 }

evalIdentityM ex
    
(* The Super Evaluator You Can't Do In F# *)
(*
let rec evalM comp e =
  match e with
    CstI i -> comp { return i }
  | Prim(op, e1, e2) ->
    comp { let! v1 = evalM e1
           let! v2 = evalM e2
           return! opEval op v1 v2 }
*)

(* From: http://en.wikibooks.org/wiki/F_Sharp_Programming/Computation_Expressions *)
open System.Threading
open System.Text.RegularExpressions

let downloadAsync (url : string) =
  let thId() = Thread.CurrentThread.ManagedThreadId
  let printMsg msg = printfn "ThreadID = %i, Url = %s, %s" (thId()) url msg
  let bind(input, rest) =
    ThreadPool.QueueUserWorkItem(new WaitCallback( fun _ -> printfn "[%i..." (thId());
                                                            let r = rest(input()) in printfn "...%i]" (thId()); r)) |> ignore
  bind( (fun () -> printMsg "Creating webclient..."; new System.Net.WebClient()),
        fun webclient -> bind( (fun () -> printMsg "Downloading url..."; webclient.DownloadString(url)),
                               fun html -> bind( (fun () -> printMsg "Extracting urls..."; Regex.Matches(html, @"http://\S+") ),
                                                  fun matches -> printMsg ("Found " + matches.Count.ToString() + " links") ) ) )  

let ex = ["http://www.google.com/"; "http://microsoft.com/"; "http://www.wordpress.com/"; "http://www.itu.dk"]
Seq.iter downloadAsync ex


(* Monadic Parsers, HR 293 *)

type PersonData = (string * (int list)) list

type Expr =
    Num of int
  | Var of string
  | Neg of Expr
  | Add of Expr * Expr
  | Sub of Expr * Expr
  | Mul of Expr * Expr

open System.Text.RegularExpressions

let nameReg = Regex @"\G\s*([a-zA-Z][a-zA-Z0-9]*)"
let numberReg = Regex @"\G\s*([0-9]+)"

let numReg = Regex @"\G\s*((?:\053|-|)[0-9]+)"
let varReg = Regex @"\G\s*([a-zA-Z][a-zA-Z0-9]*)"
let plusMinReg = Regex @"\G\s*(\053|\055)"
let addOpReg = plusMinReg
let signReg = plusMinReg
let mulOpReg = Regex @"\G\s*(\052)"
let leftParReg = Regex @"\G\s*(\050)"
let rightParReg = Regex @"\G\s*(\051)"
let eosReg = Regex @"\G\s*$"

type 'a parser = string -> int -> ('a * int) list

#r "../Lec08/TextProcessing/src/TextProcessing/bin/Debug/net8.0/TextProcessing.dll"
open TextProcessing

let token (reg:Regex) (conv: string -> 'a) : 'a parser =
  fun str pos ->
    let ma = reg.Match(str,pos)
    match ma.Success with
      | false -> []
      | _     ->
        let pos2 = pos + ma.Length
        [(conv(captureSingle ma 1), pos2)]
          
let emptyToken (reg:Regex) : unit parser =
  fun str pos ->
    let ma = reg.Match(str,pos)
    match ma.Success with
      | false -> []
      | _     ->
        let pos2 = pos + ma.Length
        [((), pos2)]

(* Token parser for the person data *)
let name = token nameReg id
let number = token numberReg int

let ex1 = "Peter 5 John 3 4"
let _ = name ex1 0
let _ = number ex1 5
let _ = name ex1 7
let _ = number ex1 12
let _ = number ex1 14

(* Token parser for the expression language *)
let numFct (str:string) = Num (int str)
let varFct = Var
let addOpFct = function
  | "+" -> fun x y -> Add(x,y)
  | _   -> fun x y -> Sub(x,y)
let mulOpFct _ = fun x y -> Mul(x,y)
let signFct = function
  | "+" -> id
  | "-" -> fun x -> Neg x

let num      = token numReg numFct
let var      = token varReg varFct
let addOp    = token addOpReg addOpFct
let mulOp    = token mulOpReg mulOpFct
let sign     = token signReg signFct
let leftPar  = emptyToken leftParReg
let rightPar = emptyToken rightParReg
let eos      = emptyToken eosReg

let ex2 = "1 + a + (32-3)*-4*3"
let _ = num ex2 0
let _ = addOp ex2 1
let _ = var ex2 3
let _ = addOp ex2 5
let _ = leftPar ex2 7
let _ = num ex2 9
let _ = addOp ex2 11
let _ = num ex2 12
let _ = rightPar ex2 13
let _ = mulOp ex2 14
let _ = sign ex2 15
let _ = num ex2 16
let _ = mulOp ex2 17
let _ = num ex2 18

(* Parser class *)
type ParserBuilder() =
  member this.Bind(p:'a parser, f: 'a -> 'b parser): 'b parser =
    fun str pos -> List.collect (fun (a,pos) -> f a str pos) (p str pos)
  member this.Zero() = (fun _ _ -> []): 'a parser
  member this.Return a = (fun _ pos -> [(a,pos)]): 'a parser
  member this.ReturnFrom (p:'a parser) = p

let parser = ParserBuilder()  

let pairOf p1 p2 =
  parser { let! x1 = p1
           let! x2 = p2
           return (x1,x2) }
let nameNumber = pairOf name number
let _ = nameNumber "name1 43 name2 22" 0
let _ = nameNumber "name" 0

let pairOfT p1 p2 =
  parser.Bind(p1,
              fun x1 -> parser.Bind(p2, fun x2 -> parser.Return (x1,x2)))
let nameNumber' = pairOfT name number
let _ = nameNumber' "name1 43 name2 22" 0
let _ = nameNumber' "name" 0

let (<|>) (p1:'a parser) (p2:'a parser) = fun str pos -> (p1 str pos) @ (p2 str pos)
let numOrVar = num <|> var
let _ = numOrVar "23 a" 0
let _ = numOrVar "23 a" 2

let rec listOf p =
  parser { return [] }
  <|> parser { let! x = p
               let! xs = listOf p
               return x::xs }

let _ = (listOf number) "3 7 8 9" 0
    
let rec infixL op q p =
  p <|>
  parser { let! a = p
           let! f = op
           let! b = q
           let a1 = f a b
           let p1 = parser { return a1 }
           return! infixL op q p1 }

let psL = infixL addOp numOrVar numOrVar
let _ = psL "a - b + 5" 0

let rec infixR op q p =
  q <|>
  parser { let! a = p
           let! f = op
           let! b = infixR op q p
           return f a b }
let psR = infixR addOp numOrVar numOrVar
let _ = psR "a - b + 5" 0

let person = pairOf name (listOf number)
let personData = listOf person
let _ = personData "John 35 2 Sophie 27 Richard 17 89 3" 0

let rec expr = infixL addOp term term
and term     = infixL mulOp factor factor
and factor = num <|>
             var <|>
             parser { let! f = sign
                      let! x = factor
                      return (f x) } <|>
             parser { let! _ = leftPar
                      let! x = expr
                      let! _ = rightPar
                      return x }
let _ = expr "-a1 + 2 * (a2 - 3)" 0

let eosParser p =
  parser { let! x = p
           let! _ = eos
           return x }

let personDataEOS = eosParser personData
let _ = personDataEOS "John 35 2 Sophie 27 Richard 17 89 3" 0

let exprEOS = eosParser expr
let _ = exprEOS "-a1 + 2 * (a2 - 3)" 0

