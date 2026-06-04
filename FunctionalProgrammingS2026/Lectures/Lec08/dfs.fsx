// DFS

type color = White | Gray | Black

let dfs(V,adj: int list[]) =
  let color        = Array.create V White
  let pi           = Array.create V -1
  let d            = Array.create V -1
  let f            = Array.create V -1
  let mutable time = 0

  let rec visit u =
    color.[u] <- Gray ; time <- time + 1; d.[u] <- time
    let rec h v = if color.[v] = White
                  then  pi.[v]  <- u
                        visit v
    List.iter h (adj.[u])
    color.[u] <- Black
    time      <- time + 1
    f.[u]     <- time

  let mutable i = 0
  while i < V do
    if color.[i] = White
    then visit i
    i <- i + 1
  (d, f, pi)

let adj =
  Array.ofList [ [1;3];
                 [4];
                 [4;5];
                 [1];
                 [3];
                 [5]]

let g6 = (6,adj)
  
let (d,f,pi) = dfs(g6)
printfn "pi = %A" pi
printfn "f = %A" f
printfn "d = %A" d
