open System.IO

type Tree =
    | Leaf
    | Node of Tree * string * Tree list

let buildTree (path: string) =
    let rec build p =
        let dirs = Directory.GetDirectories(p)
        let children = dirs |> Array.toList |> List.map build
        match children with
        | [] -> Leaf
        | _  -> Node(Leaf, p, children)
    build path

let rec countC t c =
    match t with
    | Leaf -> c 0
    | Node(_, _, children) ->
        let rec countList lst acc cont =
            match lst with
            | [] -> cont acc
            | x::xs -> countC x (fun v -> countList xs (acc + v) cont)
        countList children 0 (fun total -> c(total + 1))

let tree = buildTree @"C:\Projects"
let antal = countC tree id
printfn "Antal mapper: %d" antal


open System.IO

let antal = Directory.GetDirectories(@"C:\Projects", "*", SearchOption.AllDirectories).Length
printfn "Antal mapper: %d" antal