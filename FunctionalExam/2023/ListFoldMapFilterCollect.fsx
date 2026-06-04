
// Tag: ottobotcode ottobot otto bot code  

// List.Fold takes a list of elements and returns a single output. 
//      // fun state input -> state + input ; // repeat
List.fold (fun s i -> s + i ) "" ["K";"A";"S"] 
List.fold ( fun s i -> s*i) 1 [1;3;3]
List.fold (fun lst e -> e::lst ) [] ["k";"a";"S"] 
List.fold (fun acc x -> acc + 1) 0 [1;2;3]

open System // for random below

// List.map takes a list and returns a new list with transformed elements. 
//    // Transformation // list  
List.map ( fun x -> x*x)   [1;2;3;4] 
List.map String.length ["Kasper";"Franziska";]
List.map ( fun x -> 
            let rnd = Random()
            let value = rnd.Next(1, 101)
            string x + "_" + string value) [0;1;3;4]

// list.filter read lists as i inputs, checks i with the predicate and returns a list of elements meeting predicate criteria. 
        // input -> predicate // list  
List.filter ( fun i -> i%2=0 ) [0;1;3;4;5] 
List.filter ( fun i -> i > 0.0) [-1.9; 1.5; 3.4; 4.43; 5.3] 
List.filter ( fun (s: string) -> s.StartsWith("c")) ["capper"; "Kapper"]


// list.collect seems like a .map but it does not need to return same lenght list. 
           // input   // transformation    
List.collect (fun i -> [i*2, i*4]) [1;3;4]
List.collect (fun i -> ["_ID" + string i, "VID_" + string (i*4)])  [1;3;4]


// Tuple 
let myTuple = "Kylling", 2 
let name, age = myTuple

let myTriple = "Kasper", 37, 193.5
let TName, TAge, height = myTriple 



