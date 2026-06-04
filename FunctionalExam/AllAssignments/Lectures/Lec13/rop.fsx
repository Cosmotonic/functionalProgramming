// This source is taken from the article on Railway Oriented Programming
// by Scott W, see
// https://fsharpforfunandprofit.com/posts/recipe-part2/#the-railway-track-functions-complete-code

// Idea: A user flow is defined as a sequence of functions each doing
// a part of the work. Each part can fail and we want a nice way to
// code for the happy path (no errors) and have errors being
// collected/handled nicely as we go along.

// Represent success and error as one value:

// An example function that do a small step that may also fail.

// Generic result type incorporating both success and failure.
type Result<'TSuccess,'TFailure> =
  | Success of 'TSuccess
  | Failure of 'TFailure

type Request = {name:string; email:string}

let validateInput input =
   if input.name = "" then Failure "Name must not be blank"
   else if input.email = "" then Failure "Email must not be blank"
   else Success input  // happy path

// Converting switches to two-track inputs.

// ** Converting switches to two-track inputs **
   
// Given a function like validate that only takes a non-error input
// and turn it into a function also taking an error input which may
// happen if the function (step) before fails. In this case the error
// should just be passed on.

// The switchFunction is the function only taking the non-error input.
let bind switchFunction = 
  fun twoTrackInput -> 
    match twoTrackInput with
    | Success s -> switchFunction s
    | Failure f -> Failure f   

// Explain the type of bind.

// Check the type of below - validateInput now taking a Result-type as
// input also.
bind validateInput

// ** Example: Combining some validation functions **
   
// Bigger Example: Combining some validation functions
// Let's create some steps in the workflow.
let validate1 input =
  if input.name = "" then Failure "Name must not be blank"
  else Success input

let validate2 input =
  if input.name.Length > 50
  then Failure "Name must not be longer than 50 chars"
  else Success input

let validate3 input =
   if input.email = "" then Failure "Email must not be blank"
   else Success input

// Let's glue together the workflow
// FUNCTION ORIENTED
let combinedValidation = 
  // convert from switch to two-track input
  let validate2' = bind validate2
  let validate3' = bind validate3
  // connect the two-tracks together
  validate1 >> validate2' >> validate3'

// We use bind to turn the validateX functions into functions
// accepting a Result value as input. Afterwards we can use the normal
// >> function composition operator.

// Let's test that it works.
// Test 1
let input1 = {name=""; email=""}
combinedValidation input1 |> printfn "Result1=%A"

// Test 2
let input2 = {name="Alice"; email=""}
combinedValidation input2 |> printfn "Result2=%A"

// Test 3
let input3 = {name="Alice"; email="good"}
combinedValidation input3 |> printfn "Result3=%A"

// ** Bind as a piping operation **
// We can combine bind and >> in one operator, >>=
let (>>=) twoTrackInput switchFunction = 
  bind switchFunction twoTrackInput

// Explain the type of >>=

// This gives a new version of the combined workflow.
// DATA ORIENTED
let combinedValidation x = 
  x 
  |> validate1   // normal pipe because validate1 has a one-track input
                 // but validate1 results in a two track output...
  >>= validate2  // ... so use "bind pipe". Again the result is a two track output
  >>= validate3  // ... so use "bind pipe" again.   

// ** An alternative to bind -- combining switches into bigger switches **
// We can also define bind as an operator that glues two single input
// value functions together directly. We call this operator >=>

let (>=>) switch1 switch2 x = 
  match switch1 x with
  | Success s -> switch2 s
  | Failure f -> Failure f
  
// Explain the type of >=>

// Alternative declaration
let (>=>) switch1 switch2 = 
  switch1 >> (bind switch2)

// This gives below version of the combined workflow - as a combined switch:
let combinedValidation = 
  validate1 
  >=> validate2 
  >=> validate3   

// ** Converting simple functions to the railway oriented programming model **
// This concept can now be extended in many ways.

let canonicalizeEmail input =
  { input with email = input.email.Trim().ToLower() }  

// We can convert above normal function into a switch
let switch f x = f x |> Success
// Explain type

// We now extend the workflow
let usecase = 
  validate1 
  >=> validate2 
  >=> validate3 
  >=> switch canonicalizeEmail

// A bit of testing
let goodInput = {name="Alice"; email="UPPERCASE "}
usecase goodInput |> printfn "Canonicalize Good Result = %A"

let badInput = {name=""; email="UPPERCASE "}
usecase badInput |> printfn "Canonicalize Bad Result = %A"

// ** Creating two-track functions from one-track functions **

// Convert a normal function into a two-track function
let map oneTrackFunction twoTrackInput = 
  match twoTrackInput with
  | Success s -> Success (oneTrackFunction s)
  | Failure f -> Failure f

// The workflow then looks like this
let usecase = 
  validate1 
  >=> validate2 
  >=> validate3 
  >> map canonicalizeEmail  // normal composition

// ** Converting dead-end functions to two-track functions **
let tee f x = 
  f x |> ignore
  x

// A dead-end function  
let updateDatabase input =
  ()   // dummy dead-end function for now

// Example including a dead-end function.
let usecase = 
  validate1 
  >=> validate2 
  >=> validate3 
  >=> switch canonicalizeEmail
  >=> switch (tee updateDatabase)

// Same example with normal function composisition, >>
let usecase = 
  validate1 
  >> bind validate2 
  >> bind validate3 
  >> map canonicalizeEmail   
  >> map (tee updateDatabase)

// ** Handling exceptions **
let tryCatch f x =
  try
    f x |> Success
  with
  | ex -> Failure ex.Message
// Explain type

// Example including a tryCatch
let usecase = 
  validate1 
  >=> validate2 
  >=> validate3 
  >=> switch canonicalizeEmail
  >=> tryCatch (tee updateDatabase)

// ** Functions with two-track input, e.g., for logging. **
let doubleMap successFunc failureFunc twoTrackInput =
  match twoTrackInput with
  | Success s -> Success (successFunc s)
  | Failure f -> Failure (failureFunc f)
// Explain type
  
// Example including a two-track logging function.
let log twoTrackInput = 
  let success x = printfn "DEBUG. Success so far: %A" x; x
  let failure x = printfn "ERROR. %A" x; x
  doubleMap success failure twoTrackInput 
// Explain type of log
  
let usecase = 
  validate1 
  >=> validate2 
  >=> validate3 
  >=> switch canonicalizeEmail
  >=> tryCatch (tee updateDatabase)
  >> log

// A few tests
let goodInput = {name="Alice"; email="good"}
usecase goodInput |> printfn "Good Result = %A"
// DEBUG. Success so far: {name = "Alice"; email = "good";}
// Good Result = Success {name = "Alice"; email = "good";}

let badInput = {name=""; email=""}
usecase badInput |> printfn "Bad Result = %A"
// ERROR. "Name must not be blank"
// Bad Result = Failure "Name must not be blank"  

// ** Converting a single value to a two-track value **
let succeed x = Success x

let fail x = Failure x

// ** Combining functions in parallel **
   
let plus addSuccess addFailure switch1 switch2 x = 
  match (switch1 x),(switch2 x) with
  | Success s1,Success s2 -> Success (addSuccess s1 s2)
  | Failure f1,Success _  -> Failure f1
  | Success _ ,Failure f2 -> Failure f2
  | Failure f1,Failure f2 -> Failure (addFailure f1 f2)

// create a "plus" function for validation functions
let (&&&) v1 v2 = 
  let addSuccess r1 r2 = r1 // return first
  let addFailure s1 s2 = s1 + "; " + s2  // concat
  plus addSuccess addFailure v1 v2 

let combinedValidation = 
  validate1 
  &&& validate2 
  &&& validate3 

// Redo tests from earlier
// test 1
let input1 = {name=""; email=""}
combinedValidation input1 |> printfn "Result1=%A"
// ==> Result1=Failure "Name must not be blank; Email must not be blank"

// test 2
let input2 = {name="Alice"; email=""}
combinedValidation input2 |> printfn "Result2=%A"
// ==> Result2=Failure "Email must not be blank"

// test 3
let input3 = {name="Alice"; email="good"}
combinedValidation input3 |> printfn "Result3=%A"
// ==> Result3=Success {name = "Alice"; email = "good";}  

let usecase = 
  combinedValidation
  >=> switch canonicalizeEmail
  >=> tryCatch (tee updateDatabase)

// test 4
let input4 = {name="Alice"; email="UPPERCASE "}
usecase input4 |> printfn "Result4=%A"
// ==> Result4=Success {name = "Alice"; email = "uppercase";}  

// ** Dynamic injection of functions **
type Config = {debug:bool}

let debugLogger twoTrackInput = 
  let success x = printfn "DEBUG. Success so far: %A" x; x
  let failure = id // don't log here
  doubleMap success failure twoTrackInput 

let injectableLogger config = 
  if config.debug then debugLogger else id

let usecase config = 
  combinedValidation 
  >> map canonicalizeEmail
  >> injectableLogger config

// Test of injecting a debug function
let input = {name="Alice"; email="good"}

let releaseConfig = {debug=false}
input
|> usecase releaseConfig 
|> ignore
// no output

let debugConfig = {debug=true}
input 
|> usecase debugConfig 
|> ignore
// debug output
// DEBUG. Success so far: {name = "Alice"; email = "good";}  

