// 6.2
// Functional experssion type
type Fexpr = 
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr
    | Kas of Fexpr * Fexpr * Fexpr // test 

let rec postfix = function
    | Const r       -> string r
    | X             -> "x"
    | Add(fe1, fe2) -> postfix fe1 + " " + postfix fe2 + " +"
    | Sub(fe1, fe2) -> postfix fe1 + " " + postfix fe2 + " -"
    | Mul(fe1, fe2) -> postfix fe1 + " " + postfix fe2 + " *"
    | Div(fe1, fe2) -> postfix fe1 + " " + postfix fe2 + " /"
    | Sin fe        -> postfix fe + " sin"
    | Cos fe        -> postfix fe + " cos"
    | Log fe        -> postfix fe + " log"
    | Exp fe        -> postfix fe + " exp"
    | Kas (fe1, fe2, fe3) -> postfix (Add(fe1, fe2)) + " Kas " + postfix fe3 // test  

postfix (Sub(X, Const 7.0))
postfix (Sin(Const 7.0))
postfix (Add(Const 7.0, Const 7.0))
postfix (Kas(Const 5.0, Const 10.0, Const 7.0)) // val it: string = "5 10 + Kas 7"
postfix (Const 5.0)

// For egen forståelse. 
type kasExp = 
    | Leaf of char      // konstruktøre. 
    | Tester of kasExp  // konstruktøre. 

let rec outVal = function 
    | Leaf c    -> string c
    | Tester r  -> "Tester(" + outVal r + ")"

outVal (Tester(Leaf 'k'))  // => "Tester(k)"
// outVal (Tester(Leaf 'k')) 
// outVal (kører rekursiv metode på ( leaf of char))

// forståelse for Int 
type konst = 
    | BaseIn of int 
    | KasperAdd of konst * konst 

let rec recKonst = function 
    | BaseIn i -> i 
    | KasperAdd (va1, va2) -> recKonst va1 + recKonst va2

recKonst (KasperAdd(BaseIn 1, BaseIn 2))

// 6.8 pt 1: Stack type + intpInstr
// We consider a simple calculator with instructions for addition, subtraction, multiplication and
// division of floats, and the functions: sin, cos, log and exp.
type Instruction = 
    | ADD | SUB | MULT | DIV 
    | SIN | COS | LOG | EXP | POP
    | PUSH of float

type Stack = float list

let intpInstr (stack: Stack) (instr: Instruction) : Stack =
    match instr, stack with
    | PUSH r, _             -> r :: stack
    | POP,   a :: rest      -> rest
    | ADD,   b :: a :: rest -> (a + b) :: rest
    | SUB,   b :: a :: rest -> (a - b) :: rest
    | MULT,  b :: a :: rest -> (a * b) :: rest
    | DIV,   b :: a :: rest -> (a / b) :: rest
    | SIN,   a :: rest      -> (sin a) :: rest
    | COS,   a :: rest      -> (cos a) :: rest
    | LOG,   a :: rest      -> (log a) :: rest
    | EXP,   a :: rest      -> (exp a) :: rest
    | _                     -> failwith "intpInstr: invalid stack state"
let stack1 = intpInstr [] (PUSH 5.0)      // [5.0]
let stack2 = intpInstr stack1 (PUSH 3.0)  // [3.0; 5.0]
intpInstr stack2 (POP)
let stack3 = intpInstr stack2 (PUSH 3.0)  // [3.0; 5.0]
let stack4 = intpInstr stack3 ADD 

// 6.8 pt 2: Program to output first value. 
// A program for the calculator is a list of instructions [i1, i2, ..., in]. A program is executed
// by executing the instructions i1, i2, ..., in one after the other, in that order, starting with an empty stack. 
// The result of the execution is the top value of the stack when all instructions have been executed.
// Declare an F# function to interpret the execution of a program: intpProg: Instruction list -> float
let rec firstOutput : float = 
    let calculatedStack = List.fold(intpInstr) [] [PUSH 5.0; PUSH 3.0; ADD; PUSH 5.0; PUSH 3.0] // As a for loop (..ish, try not to compare too much)
    match calculatedStack with 
    | a::rest -> a

firstOutput

// 6.8 pt 3: Partial application — giv en funktion færre argumenter end den forventer, og få en ny funktion tilbage der venter på resten.
// Vi skal lave en liste af de instrukser
let rec transform (fexpression: Fexpr) (x: float) : Instruction list =
    match fexpression with
    | Const r -> [PUSH r]
    | X       -> [PUSH x]
    | Add(fe1, fe2) -> transform fe1 x @ transform fe2 x @ [ADD]
    | Sub(fe1, fe2) -> transform fe1 x @ transform fe2 x @ [SUB]
    | Mul(fe1, fe2) -> transform fe1 x @ transform fe2 x @ [MULT]
    | Div(fe1, fe2) -> transform fe1 x @ transform fe2 x @ [DIV]
    | Sin fe        -> transform fe x @ [SIN]
    | Cos fe        -> transform fe x @ [COS]
    | Log fe        -> transform fe x @ [LOG]
    | Exp fe        -> transform fe x @ [EXP]

transform (Sub(X, Const 7.0)) 50.0

// 1. matcher Sub(fe1, fe2) hvor fe1 = X, fe2 = Const 7.0
// 2. transform X 50.0 -> [PUSH 50.0]               // X pattern  
// 3. transform (Const 7.0) 50.0 -> [PUSH 7.0]      // Const r pattern (ignore 50.0) Partial application
// 4. sæt listerne sammen med @: [PUSH 50.0] @ [PUSH 7.0] @ [SUB]
// 5. resultat: [PUSH 50.0; PUSH 7.0; SUB]
//
// intpProg kører det bagefter:
// []  -> PUSH 50.0 -> [50.0]
// [50.0] -> PUSH 7.0 -> [7.0; 50.0]
// [7.0; 50.0] -> SUB (a-b = 50.0-7.0) -> [43.0]
// returner toppen: 43.0

// My own mess below. 
type Complex1 =
    | Const of float
    | Cpx of float * float 

let rec make ( in1 : float) (complex : Complex1) : float = 
    match complex with 
    | Const r                 -> string r
    | Cpx(in1, in2)           -> make (in1 Const) * make (in2 Const)



(*
// My own practice // *** if Hannes and Marc finds his comment ill buy you a beer.  
type StringStack = string list
type combiWordsTyp = 
    | COMB  
    | PUSH of string 
    | RMV
let combineWords (stringStack : StringStack) (instr : combiWordsTyp) : StringStack = 
    match instr, stringStack with 
    | PUSH w, _                     -> w :: stringStack 
    | COMB, b :: a :: rest -> (a + " " + b) :: rest
    | RMV,  a :: rest               -> rest
let words1 = combineWords [] (PUSH "hello")
let words2 = combineWords words1 (PUSH "Kasper")
let words3 = combineWords words2 RMV
let words4 = combineWords words3 (PUSH "Franzi")
combineWords words4 COMB

*)


// UDEN akkumulator
let rec fact n =
    match n with
    | 0 -> 1
    | n -> n * fact(n-1) // multiply * happens before the recursive call = UDEN akkumulator
    
// fact 4
// 4 * fact 3
// 4 * 3 * fact 2
// 4 * 3 * 2 * fact 1
// 4 * 3 * 2 * 1        <- først HER kan vi regne

// MED akkumulator 
let rec factA n m =
    match n with
    | 0 -> m
    | n -> factA (n-1) (n*m) // multiply * happens after the recursive call = MED akkumulator

// factA 4 1
// factA 3 4            <- 4 gemt i m
// factA 2 12           <- 12 gemt i m
// factA 1 24           <- 24 gemt i m
// factA 0 24           <- returner m = 24