// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 10: Text processing programs
// All programs except those from the keyword-index program 


// From Section 10.2 Capturing data using regular expressions

open System.Text.RegularExpressions
open System.Globalization
#r @"./TextProcessing/src/TextProcessing/bin/Debug/net8.0/TextProcessing.dll"
open TextProcessing

// Slide 4
// Keyword index example
let reg =
    Regex @"\G\s*\042([^\042]+)\042(?:\s+([^\s]+))*\s*$"


let m = reg.Match "\"Control.Observable Module (F#)\" observer event~observer"

m.Success

captureSingle m 1

captureList m 2

let tildeReg = Regex @"~"
let tildeReplace str = tildeReg.Replace(str," ")

tildeReplace "event~observer"

// Slide 5
// Nested data, page 226
let regNest =
    Regex @"\G(\s*([a-zA-Z]+)(?:\s+(\d+))*)*\s*$"

// Capture in two steps, page 226
    
let regOuter = Regex @"\G(\s*[a-zA-Z]+(?:\s+\d+)*)*\s*$"

let m1 = regOuter.Match " John 35 2 Sophie 27 Richard 17 89 3 "

captureList m1 0
captureList m1 1
captureCountList m1

// Slide 6
let regPerson1 =
    Regex @"\G\s*([a-zA-Z]+)(?:\s+(\d+))*\s*$"

let p1 = regPerson1.Match " John 35 2 "
captureCountList p1
captureList p1 0
captureList p1 1
captureList p1 2

let extractPersonData subStr =
  let m = regPerson1.Match subStr
  (captureSingle m 1, List.map int (captureList m 2))


let getData1 str =
  let m = regOuter.Match str
  match (m.Success) with
  | false -> None
  | _     ->
      Some (List.map extractPersonData (captureList m 1))

getData1 " John 35 2 Sophie 27 Richard 17 89 3 "

// Using successive calls of Match, page 227

let regPerson2 =
   Regex @"\G\s*([a-zA-Z]+)(?:\s+(\d+))*\s*"

let m2 =
  regPerson2.Match
    (" John 35 2 Sophie 27 Richard 17 89 3 ", 11)

captureSingle m2 1

captureList m2 2

m2.Length 

let rec personDataList str pos top =
  if pos >= top then Some [] 
  else let m = regPerson2.Match(str,pos)
       match m.Success with
       | false -> None
       | true  -> let data = (captureSingle m 1,
                              List.map int (captureList m 2))
                  let newPos = pos + m.Length
                  match (personDataList str newPos top) with
                  | None     -> None
                  | Some lst -> Some (data :: lst)

let getData2 (s: string) = personDataList s 0 s.Length
getData2 " John 35 2 Sophie 27 Richard 17 89 3 "


(* NH 2024-04-02: Deprecated.

// From Section 10.4 File handling. Save and restore values in files

// Needs referencing and opening the TextProcessing Module 
let v1 = Map.ofList [("a", [1..3]); ("b", [4..10])]

saveValue v1 "v1.bin"

let x = 3
let v2 = [(fun x-> x+3); (fun x -> 2*x*x); (fun z -> z+x)]
saveValue v2 "v2.bin"
 
let value1:Map<string,int list> = restoreValue "v1.bin"

let [f;g;z]: (int->int) list = restoreValue "v2.bin"
f 7
g 2
z 3

let r = ref 0
let v3 () = (r := !r + 1; !r)
v3()

saveValue v3 "v3.bin"
let v3':(unit->int) = restoreValue "v3.bin"

v3'()

// Try restore the value v3' in existing interactive and in a new
// interactive.  Crashes in new interactive because reflection is
// happening behind the scenes and the reference r does not exist in
// the new interactive


// From Section 10.6 Culture-dependent information. String orderings
*)

// Slide 8
let SpanishArgentina = CultureInfo "es-AR"

let printCultures () =
  Seq.iter
    (fun (a:CultureInfo) ->
          printf "%-12s %s\n" a.Name a.DisplayName)
    (CultureInfo.GetCultures(CultureTypes.AllCultures))

printCultures()

// Slide 10
let svString = orderString "sv-SE"

let dkString = orderString "da-DK"

// Swedish alphabet: A B C D E F G H I J K L M N O P Q R S T U V W X Y Z Å Ä Ö
svString "ö" < svString "å"
svString "ö" > svString "å"

dkString "ø" < dkString "å"
dkString "ø" > dkString "å"

dkString "a" < svString "b"

let str = svString "abc"
string str

orderCulture str

// Slide 11
let enString = orderString "en-US"
let enListSort lst =
    List.map string (List.sort (List.map enString lst))

enListSort ["Ab" ; "ab" ; "AC" ; "ad" ]

enListSort ["a"; "B"; "3"; "7"; "+"; ";"]

enListSort ["multicore";"multi-core";"multic";"multi-"]

// Slide 12
orderCulture (enString "Hi There")

// Slide 14
(* Folding over fileFold.txt *)

let file = "fileFold.txt"
let sum = fileFold (fun a s -> a + (int s)) 0 file
let sum2 = fileXfold (fun a rdr -> a + (int (rdr.ReadLine()))) 0 file
let ppRows = fileIter (printfn "%s") file
let ppRows2 = fileXiter (fun rdr -> printfn "%s" (rdr.ReadLine())) file
