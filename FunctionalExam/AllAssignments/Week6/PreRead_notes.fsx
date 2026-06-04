type Animal(noiseMakingStrategy) =
   member this.MakeNoise =
      noiseMakingStrategy() |> printfn "Making noise %s"

// now create a cat
let meowing() = "Meow"
let cat = Animal(meowing)
cat.MakeNoise

// .. and a dog
let woofOrBark() = if (System.DateTime.Now.Second % 2 = 0)
                   then "Woof" else "Bark"
let dog = Animal(woofOrBark)
dog.MakeNoise
dog.MakeNoise  //try again a second later





let eachAdd1 = List.map (fun i -> i+1)
eachAdd1 [0;1;2;3]

let excludeOneOrLess = List.filter (fun i -> i>1)
excludeOneOrLess [0;1;2;3]

let sortDesc = List.sortBy (fun i -> -i)
sortDesc [0;1;2;3]


// commonly accepted guidelines:
// Put earlier: parameters more likely to be static
// Put last: the data structure or collection (or most varying argument)
// For well-known operations such as “subtract”, put in the expected order

// piping using list functions
let result =
  [1..10]
  |> List.map (fun i -> i+1)
  |> List.filter (fun i -> i>5)
// output => [6; 7; 8; 9; 10; 11]

let f1 = List.map (fun i -> i+1)
let f2 = List.filter (fun i -> i>5)
let compositeOp = f1 >> f2 // compose
let result1 = compositeOp [1..10]
// output => [6; 7; 8; 9; 10; 11]



// create wrappers for .NET string functions
let replace oldStr newStr (s:string) =
  s.Replace(oldValue=oldStr, newValue=newStr)

let startsWith (lookFor:string) (s:string) =
  s.StartsWith(lookFor)

let result2 =
  "hello"
  |> replace "h" "j"
  |> startsWith "j"

["the"; "quick"; "brown"; "fox"]
  |> List.filter (startsWith "qu")

["the"; "quick"; "brown"; "fox"]
  |> List.filter (startsWith "f")

let compositeOp1 = replace "h" "j" >> startsWith "j"
let result3 = compositeOp1 "hello"

printf "%i" <| 1+2       // using reverse pipe
printf "%i" (1+3)        // using parens

let add x y = x + y
// (1+2) add (3+4)          // error
1+2 |> add <| 3+4        // pseudo infix



// Class notes. 


