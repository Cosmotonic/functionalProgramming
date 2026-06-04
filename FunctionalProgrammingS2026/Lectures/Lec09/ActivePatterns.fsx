// Source of examples:
//   https://docs.microsoft.com/en-us/dotnet/articles/fsharp/language-reference/active-patterns

let (|Even|Odd|) input =
  if input % 2 = 0 then Even else Odd

let TestNumber input =
   match input with
   | Even -> printfn "%d is even" input
   | Odd -> printfn "%d is odd" input

TestNumber 7
TestNumber 11
TestNumber 32

let (|Int|_|) (str:string) =
  match System.Int32.TryParse(str) with
  | (true,i) -> Some i
  | _ -> None

let (|Bool|_|) (str:string) =
  match System.Boolean.TryParse(str) with
  | (true,b) -> Some b
  | _ -> None

let testParse str = 
  match str with
  | Int i -> printfn "The value is an int '%i'" i
  | Bool b -> printfn "The value is a bool '%b'" b
  | _ -> printfn "The value '%s' is something else" str

testParse "12"
testParse "true"
testParse "abc"
