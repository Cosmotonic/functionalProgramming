

// 1.1. 
let noRemainderP (m:int) (n:int) : bool = 
    if m % n = 0 then true else false 

let checkNumber (n:int) (m:int) : bool = 
    let toCheck = [1..n] 
    toCheck |> List.forall (fun x -> noRemainderP m x ) 

checkNumber 11 2520 

let untilTrue (f:int->bool) : int = 
    let rec aux f s = 
        if (f s) then s  
        else (aux f (s+1))
    aux f 1
    
untilTrue (fun n -> n % 4 = 0)
// untilTrue (fun _ -> false)

let findSmallest (n:int) : int = 
    untilTrue (checkNumber n)  

findSmallest 3

// 1.2 
let rec revApped (xs:'a list) (ys:'a list) : 'a list = 
    match xs with 
    | [] -> ys
    | h::tail -> 
            let revList = revApped tail (h::ys) 
            revList

revApped [1;2] [3;4]

let average (xs:float list) : float = 
    let rec aux (xs:float list) acc s = 
        match xs with 
        | [] -> acc/s 
        | h::t -> aux t (acc+h) (s+1.0)
    aux xs 0 0 
average [1.0;2.0]


let rec maxBy (f:'a->'b) (xs:'a list) : 'a when 'b : comparison = 
    match xs with 
    | [] -> failwith "failure" 
    | [x] -> x 
    | x::rest -> 
                let best = maxBy f rest 
                if f x > f best then x 
                else best 

maxBy ((+) 1 ) [1..9]

let rec collect (f:'a-> 'b list )  = function
    [] -> []
    | x::xs -> f x @ collect f xs

collect id [[1;2];[3;4]]

// its not tail recursive because the last thing that is dont not a call to the collect.
//  its adding the result to a list. 

let collect1 (f:'a -> 'b list) (xs:'a list) : 'b list =
    let rec aux xs acc =
        match xs with
        | [] ->
            acc |> List.rev  |> List.collect id
        | h::rest ->
            aux rest (f h :: acc)

    aux xs []
collect1 id [[1;2];[3;4]]



// 2. 
type 'a diffList = 
    LISTS of 'a list list 
    | SEQ of 'a diffList * 'a diffList

let exDF01 = SEQ ( LISTS [[1;2];[3;4]], 
                   SEQ (LISTS [[];[5]], LISTS[[6;7];[8]]))
let exDF02 = SEQ ( LISTS [[1;2];[3]], 
                   SEQ (LISTS [[];[5]], 
                   SEQ (LISTS[[6;7];[8]], LISTS [[4];[]])))

let mkDiffList : 'a diffList = SEQ(LISTS[[];[]], LISTS[[];[]])
let mkDiffList1: 'a diffList = LISTS[[];[]]

let fromList (xs:'a list) : 'a diffList = 
    let lst = LISTS[xs]
    lst 

fromList [1;2] 

let rec appendList (xs:'a list) (diffList: 'a diffList) : 'a diffList = 
    SEQ ( diffList, LISTS [xs])

appendList [1;2] (fromList [3;4]) 

let rec append (diffXs:'a diffList) (diffYs: 'a diffList) : 'a diffList = 
    SEQ (diffXs, diffYs)
append (fromList [1;4]) (fromList [3;4]) 

let rec flatten (diffXs:'a diffList) : 'a list = 
    match diffXs with 
    | LISTS x -> 
                x |> List.collect (fun y -> y) 
    | SEQ (x,y) -> 
                let flatX = flatten x 
                let flatY = flatten y
                flatX @ flatY

flatten exDF01


let rec map (f:'a->'b) (diffXs:'a diffList) : 'b diffList = 
    match diffXs with 
    | LISTS x -> 
                let collection = x |> List.map (fun x -> x|> List.map (fun y -> f y)) 
                LISTS collection 
    | SEQ (x,y) -> 
                let seqX = map f x 
                let seqY = map f y
                SEQ (seqX, seqY)

map ((+)1) exDF01
flatten(map ((+)1) exDF01)
// let value = ((*)3) 5 


// 3 
type Currency = 
    | DKK 
    | EUR 
    | USD
type TransactionType = 
    | Deposit 
    | Withdrawal
type Transaction = {
    date: int * int * int;
    transType: TransactionType;
    currency: Currency;
    amount: float }
type Account = {
    number: string;
    owner: string;
    transactions: Transaction list }
let exTransData01 = [(Deposit, DKK, 45.5, (1,1,2024));
                     (Withdrawal, DKK, 90.0, (1,2,2024));
                     (Deposit, USD, 10.0, (14,2,2024));
                     (Withdrawal, USD, 5.0, (14,1,2024));
                     (Deposit, EUR, 42.0, (16,2,2024))]
let mkTrans ((transType, currency, amount,(day,month,year)):TransactionType * Currency * float * (int * int * int)) : Transaction = 
    let trans = { 
                    amount   = amount
                    currency = currency 
                    date     = (day,month,year)
                    transType= transType 
                }
    trans 
let exTrans01 = List.map mkTrans exTransData01

let createAcc (accNum: string) (owner: string) (transactions:Transaction list) : Account = 
    let acc =  {
                    number       = accNum
                    owner        = owner
                    transactions = transactions
                }
    acc 

let exAccount01 = createAcc "001" "Hans" exTrans01

let addTransToAcc (account:Account) (transaction:Transaction) : Account = 
    let transList = account.transactions
    let newTransList = transaction::transList 
    let acc =  {
                    number       = account.number
                    owner        = account.owner
                    transactions = newTransList
                }
    acc 
let exAccount02 = addTransToAcc exAccount01 (mkTrans (Deposit, USD, 10.0, (3,2,2024)))


let deposit (account:Account) (currency:Currency) ((day, month,year):int*int*int) (amount:float) : Account = 
    let trans = mkTrans (Deposit, currency,amount,(day, month,year))
    addTransToAcc account trans

deposit exAccount01 DKK (13, 2, 2024) 42.1


type Balances = Map<Currency, float>

let getBalance (currency:'a) (balances:Map<'a, float>) : float when 'a : comparison = 
    match balances |> Map.tryFind currency with 
    | Some v -> v
    | None -> 0.0

getBalance USD ( Map [(DKK, 42.0);(USD, 43.25)])


let addToBalance (balances:Map<'a,float>) ((currency:'a), (transType:TransactionType),(amount:float)) : Map<'a,float> = 
    // we have a balance - we either add or subtract based on transtype 
    let CurrAmount = getBalance currency balances
    let newAmount = if transType = Deposit then CurrAmount + amount
                                    else CurrAmount - amount
    balances |> Map.add currency newAmount
let bal = addToBalance Map.empty (DKK, Deposit, 100.0)
let bal2 = addToBalance bal (USD, Deposit, 200.0)


let getBalancesOfAccount (account:Account) : Map<Currency,float> = 
    let allTrans = account.transactions
    let accountBal = allTrans |> List.fold (fun acc x -> addToBalance acc (x.currency, x.transType, x.amount)) Map.empty:Map<Currency,float>  
    accountBal

getBalancesOfAccount exAccount01;;



// 4 

let fizzBuzz (n:int) : string = 
    if n % 5 = 0 && n % 3 = 0 then "FizzBuzz" 
    else if n % 5 = 0 then "Buzz" 
    else if n % 3 = 0 then "Fizz"
    else sprintf "%s" (string n) 

fizzBuzz 7

let fizzBuzzSeq : string seq = 
                    Seq.initInfinite (fun n -> fizzBuzz n )
Seq.take 4 fizzBuzzSeq 

let fizzBuzzSeqCache = Seq.cache fizzBuzzSeq
let fizzBuzzSeq2 : (int * string) seq = 
    let mapped = fizzBuzzSeqCache |> Seq.mapi (fun i x -> i, x) 
    mapped |> Seq.filter (fun (i, x) -> x = "Fizz" || x = "Buzz" || x = "FizzBuzz")
                
Seq.take 4 fizzBuzzSeq2






///////////////////////////
/// 
/// 
