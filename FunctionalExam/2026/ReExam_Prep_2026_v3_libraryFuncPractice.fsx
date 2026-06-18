type FileSystem =
    | Folder of string * FileSystem list
    | File of string

let disk1 =
    Folder("C:",
        [ Folder("Documents",
            [ File("cv.pdf")
              File("budget.xlsx") ])

          Folder("Pictures",
            [ File("dog.jpg")
              File("vacation.png") ])

          File("readme.txt") ])


let rec countFiles (fs:FileSystem) : int = 
    match fs with 
    | File x -> 1 
    | Folder (_, fs) -> List.sumBy countFiles fs  

countFiles disk1


type Book =
    { Title : string
      Pages : int }

let books =
    [ { Title = "Hobbit"; Pages = 310 }
      { Title = "Dune"; Pages = 540 }
      { Title = "Foundation"; Pages = 255 } ]

let totalPages (books: Book list) : int = 
    books |> List.sumBy (fun book -> book.Pages) 

totalPages books

type Movie =
    { Title : string
      Rating : float }

let movies =
    [ { Title = "Dune"; Rating = 8.5 }
      { Title = "Alien"; Rating = 8.4 }
      { Title = "Batman & Robin"; Rating = 3.8 }
      { Title = "Arrival"; Rating = 7.9 } ]

let goodMoviesTotalScore (mov:Movie list) : float = 
    mov |> List.filter (fun x -> x.Rating > 7.0) |> List.sumBy (fun x -> x.Rating)
    
    // List.sumBy (fun movie -> if movie.Rating > 7.0 then movie.Rating else 0)
goodMoviesTotalScore movies


let goodMoviesAmount (mov:Movie list) : int = 
    mov |> List.filter (fun x -> x.Rating > 7.0) 
        |> List.length //  List.fold (fun acc x -> acc + 1 ) 0 
    
goodMoviesAmount movies

let movieTitles (movies: Movie list) : string list = 
    movies |> List.map (fun x -> x.Title)

movieTitles movies


// 
type FolderTree1 =
    | Folder of string * FolderTree1 list
    | File of string

let disk2 =
    Folder("C:",
        [ Folder("Documents",
            [ File("cv.pdf")
              File("budget.xlsx") ])

          Folder("Pictures",
            [ File("dog.jpg") ]) ])


let rec numItems (ft: FolderTree1) : int =
    match ft with 
    | Folder (str, tree) -> 1 + List.sumBy numItems tree   
    | File x -> 1

numItems disk2 

// 1 
let sales =
    [ ("TV", 5000)
      ("Laptop", 8000)
      ("Mouse", 200) ]

let totalSales (sales : (string * int) list) : int =
    sales |> List.sumBy (fun (name,price) -> price )  

totalSales sales 

// 2 
type Student =
    { Name : string
      Points : int }

let students =
    [ { Name = "Anne"; Points = 10 }
      { Name = "Bo"; Points = 15 }
      { Name = "Carl"; Points = 20 } ]

let totalPoints (students : Student list) : int =
    students |> List.sumBy (fun x -> x.Points)

totalPoints students

// 3 
type Product =
    { Name : string
      Price : int
      InStock : bool }

let products =
    [ { Name = "TV"; Price = 5000; InStock = true }
      { Name = "Laptop"; Price = 8000; InStock = false }
      { Name = "Mouse"; Price = 200; InStock = true } ]

let stockValue (products : Product list) : int =
    products |> List.filter (fun x -> x.InStock = true) 
             |> List.sumBy (fun x -> x.Price)

stockValue products

// 4 
type Folder =
    { Name : string
      Files : string list }

let folders =
    [ { Name = "Documents"
        Files = ["a.txt"; "b.txt"; "c.txt"] }

      { Name = "Pictures"
        Files = ["dog.jpg"; "cat.jpg"] } ]

let totalFiles (folders : Folder list) : int =
    folders |> List.sumBy (fun x -> x.Files |> List.length)

totalFiles folders 


type Department =
    { Name : string
      Employees : string list }

let deps =
    [ { Name = "IT"
        Employees = ["Bob"; "Peter"; "Ulla"] }

      { Name = "Finance"
        Employees = ["Hans"] }

      { Name = "Management"
        Employees = ["Hanne"; "Kirsten"; "John"] } ]


let totalEmployees (deps : Department list) : int =
    deps |> List.sumBy ( fun x -> x.Employees |> List.length) // return biggest dep
totalEmployees deps

let largestDepartment (deps : Department list) : int =
    deps |> List.fold (fun acc depC -> if acc > (depC.Employees |> List.length) then acc else depC.Employees |> List.length ) 0 
largestDepartment deps

let largestDep (deps: Department list) : int = 
    let largest = deps |> List.maxBy (fun dep -> dep.Employees |> List.length)  
    largest.Employees |> List.length 
largestDep deps

// 
type FolderTree =
    | Folder of string * FolderTree list
    | File of string

let disk =
    Folder("C:",
        [ Folder("Documents",
            [ File("cv.pdf")
              File("budget.xlsx") ])

          Folder("Pictures",
            [ Folder("Vacation",
                [ File("dog.jpg") ]) ]) ])

let rec maxDepth (ft : FolderTree) : int =
    match ft with
    | File _ -> 1 // when a file is reached we know we are at full depth
    | Folder (_, children) ->
                let depths = List.map maxDepth children
                1 + List.max depths

maxDepth disk




// 2 
type RoseTree<'T> =
    | Node of 'T * RoseTree<'T> list
    | Leaf of 'T
type Employee = string * string // (Name, Title)

let org =
    Node(("Hanne","CEO" ),
        [ Node(("Bob","CTO"),
            [Node(("Peter","Developer"),[]);
            Node(("Ulla","Developer"),[])]);
        Node(("Kirsten","CFO"),
            [Node(("Hans","Accountant"),[])])])

let org2 =
    Node(("Kurt","CEO"),
        [ Node(("Hanne","CFO"),
            [ Node(("Ulla","Accountant"),
                [ Leaf("Peter","Leder") ]) ])

          Node(("Hans","CTO"),
            [ Node(("Kurt","Manager"),
                [ Node(("PIa","Developer"),
                    [ Leaf("Kapper","Nyboss") ]) ]) ]) ])
let org3 =
    Node(("Hanne","CEO"),
        [ Node(("Bob","CTO"),
            [ Node(("Peter","Lead Developer"),
                [ Node(("Mads","Developer"),[])
                  Node(("Sofie","Developer"),[]) ])

              Node(("Ulla","Developer"),[]) ])

          Node(("Kirsten","CFO"),
            [ Node(("Hans","Senior Accountant"),
                [ Node(("Lars","Accountant"),[]) ]) ])

          Node(("Eva","COO"),
            [ Node(("Martin","Manager"),
                [ Node(("Nina","Team Lead"),
                    [ Node(("Jonas","Employee"),[]) ]) ]) ]) ])
let org8 =
    Node(("CEO","Level1"),
        [ Node(("VP","Level2"),
            [ Node(("Director","Level3"),
                [ Node(("Manager","Level4"),
                    [ Node(("Lead","Level5"),
                        [ Node(("Senior Dev","Level6"),
                            [ Node(("Developer","Level7"),
                                [ Node(("Junior Dev","Level8"),[]) ]) ]) ]) ]) ]) ])

          Node(("CFO","Level2"),
            [ Node(("Accountant","Level3"),[]) ]) ])
            
let rec numValues rt =
    match rt with
    | Node (_, children) -> 1 + List.sumBy numValues children
    | Leaf _ -> 1


let layers (rt:RoseTree<'a>) : int = 
    let rec aux rt : int = 
        match rt with 
        | Node (_, []) -> 1 
        | Node (_, rtList) -> let mappedLengths = rtList |> List.map (fun roset  -> aux roset+1) 
                              List.max mappedLengths
        | Leaf _ -> 1

    aux rt

layers org
layers org8
layers (Leaf ("Kapper","Nyboss"))

let largestDep1 (deps: Department list) : int = 
    let largest = deps |> List.maxBy (fun dep -> dep.Employees |> List.length)  
    largest.Employees |> List.length 
largestDep deps