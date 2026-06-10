
type stack<'b> = list<'b>

type mapStack<'a,'b> = 
    MapStack of list<'a*stack<'b>>

let ex1 = MapStack [('1',[3;1;1]); ('2',[1;3;1]); ('3',[1;1;3]);('4',[3;3;1]); ('5',[2;3;4]); ('6',[2;1;2])]

// 1.1.1 
let ex2 = MapStack [("Hey", ['H';'E';'Y']);("there", ['t';'h';'e';'r';'e'])]; 


type menneske<'a, 'b> = 'a * list<'b> 
type family<'a,'b> = 
        Family of list<menneske<'a, 'b>>
        // | CloseFamily of 'a * list<menneske<'a,'b> // not integrated. 


let fam1 = Family [("kasper", ["Larsson"]); ("Franziska", ["Winter"]); ("Ophelia", ["Winter";"Larsson"]); ("John", [])]
let famEm = Family [] 

// print all last names of the family 
let rec firstNames (Family fam)  : 'a list = 
    match fam with 
    | [] -> []
    | (f, x)::rest -> 
            f::firstNames (Family rest)
firstNames fam1

// print all last names 
let rec lastNames (Family fam) : 'a list = 
    match fam with 
    | [] -> []
    | (_, ln)::rest -> 
            ln @ lastNames (Family rest)  
lastNames fam1 

// count family members 
let familyCounter (Family fam ) = 
    List.length (fam) 
familyCounter fam1 

// count last names 
let rec lastNameCounter (Family fam) = 
    match fam with 
    | [] -> 0 
    | (_,lastNames)::rest -> 
                        let curLastN = List.length (lastNames)
                        curLastN + lastNameCounter (Family rest) 
lastNameCounter fam1 

// pop last name from family member 
let rec popLastName (lastName:'b) (Family fam) : family<'a,'b> = 
    match fam with 
    | [] -> Family []  
    | (fn, ln)::rest -> 
                    let keepers = ln |> List.filter (fun x -> x <> lastName )
                    let (Family newRest) = popLastName lastName (Family rest) 
                    Family ((fn, keepers)::newRest)
popLastName "Larsson" fam1 

// Add new family member with a new name 
let rec addNew (fn:'a) (ln:'b list) (Family fam) : family<'a, 'b> = 
    match fam with 
    | [] -> Family [(fn,ln)]
    | (f,l)::rest -> 
                if f = fn then raise (System.Exception "You already have a family member by that name.")
                else 
                let (Family redoRest) = addNew f l (Family rest) 
                Family ((fn,ln)::redoRest)
let fam2 = addNew "Olivia" ["Winter"] fam1 
addNew "Olivia" ["Winter"] fam2


let rec findBastards (Family first) : family<'a, 'b> = 
    match first with 
    | [] -> Family [] 
    | (fn,ln)::rest -> 
                    if (ln |> List.length ) = 0 then 
                        let (Family checkRest) = findBastards (Family rest) 
                        Family ((fn,["Snow"])::checkRest) 
                    else 
                        let (Family checkRest) = findBastards (Family rest)
                        Family  ((fn, ln)::checkRest) 
findBastards fam1  


