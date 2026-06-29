
let noRemainder (m:int) (n:int) : bool = 
    if m % n = 0 then true else false 

let checkNumber (n:int) (m:int) : bool = 
    let lst = [1..n] 
    lst |> List.forall (fun x -> noRemainder m x) 

checkNumber 10 2520 
checkNumber 11 2520 

let untilTrue (f:int->bool) : int = 
    let rec aux acc = 
        if (f acc) then acc else aux (acc+1) // check number 3 1 
    aux 1
untilTrue (fun n -> n % 3 = 0)

let findSmallest (n:int) : int = 
    untilTrue (checkNumber n)

findSmallest 3 


let rec revAppend (xs:'a list) (ys:'a list) : 'a list = 
    match xs with 
    | [] -> ys 
    | h::t -> revAppend t (h::ys) 

revAppend [1;2] [3;4]

let rec average (xs:float list) : float = 
    let rec aux xs counter acc = 
        match xs with 
        | [] -> acc / counter
        | h::t -> aux t (counter+1.0) (acc + h) 
    aux xs 0.0 0.0 
average [1.0;2.0]


let rec maxBy (f:'a->'b) (xs:'a list) : 'a when 'b: comparison= 
    match xs with 
    | [] -> failwith "maxBy: empty list" 
    | [x] -> x 
    | h::t ->  
            let bigger = maxBy f t 
            if f h > f bigger then h else bigger 
maxBy ((+)1) [1..3]


let collect (f:'a-> 'b list) (xs:'a list) : 'b list = 
    xs |> List.collect (fun x -> f x ) 

collect id [[1;2];[3;4]]


type 'a diffList =
    LISTS of 'a list list
    | SEQ of 'a diffList * 'a diffList


let exDF01 = SEQ (LISTS [[1;2];[3;4]],SEQ (LISTS [[];[5]],LISTS [[6;7];[8]]))

let exDF02 : int diffList = SEQ (SEQ (LISTS [[];[5]],LISTS [[6;7];[8]]),LISTS [[1;2];[3;4]])

let mkDiffList : 'a diffList = SEQ (LISTS [[];[]], LISTS[[];[]]) 

let fromList (xs: 'a list) : 'a diffList = 
    LISTS [xs;[]]

let item = fromList [1;2;3]

let appendList (xs:'a list) (diffList:'a diffList) : 'a diffList = 
    SEQ (diffList, LISTS [xs])

appendList [1;2] (fromList [3;4]) 

let append (diffXs:'a diffList) (diffYs:'a diffList) : 'a diffList = 
    SEQ (diffXs, diffYs) 
append (fromList [1;2]) (fromList [3;4])


let rec flatten (diffXS: 'a diffList) : 'a list = 
    match diffXS with 
    | LISTS x -> x |> List.collect (fun z -> z) 
    | SEQ (sx,sy) ->  flatten sx @ flatten sy 
flatten exDF01 


let rec map (f:'a -> 'b) (diffXs: 'a diffList) : 'b diffList = 
    match diffXs with 
    | LISTS x ->  let newList =  x |> List.map (fun x -> x |> List.map (fun x -> f x))
                  LISTS newList
    | SEQ (sx, sy) -> 
                SEQ (map f sx, map f sy)
flatten (map ((+)1) exDF01) 


// 3 
type Currency = DKK | EUR | USD

type TransactionType = Deposit | Withdrawal 

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

let mkTrans ((transType:TransactionType), (currency:Currency), (amount:float), ((day:int), (month:int),(year:int))) : Transaction = 
    let Transaction = {
        date = (day, month,year)
        transType = transType
        currency = currency
        amount = amount    }
    Transaction

let exTrans01 = List.map mkTrans exTransData01 

let createAcc (accNum:string) (owner:string) (transactions:Transaction list) : Account = 
    let acc = {
        number = accNum
        owner = owner
        transactions = transactions 
    }
    acc 

let exAccount01 = createAcc "001" "Hans" exTrans01;;

let addTransToAcc (account:Account) (transaction: Transaction ) : Account = 
    let newTransList = transaction::account.transactions 
    createAcc account.number account.owner newTransList
let exAccount02 = addTransToAcc exAccount01 (mkTrans (Deposit, USD, 10.0, (3,2,2024)));;


let deposit (account:Account) (currency:Currency) ((day:int), (month:int), (year:int)) (amount:float): Account =  
    let transfer = mkTrans (Deposit, currency, amount, (day,month,year)) 
    addTransToAcc account transfer

deposit exAccount01 DKK (13, 2, 2024) 42.1 

type Balances = Map<Currency, float>

let getBalance (currency:'a) (balances:Map<'a, float>) : float when 'a : comparison = 
    match Map.tryFind currency balances with 
    | Some value -> value 
    | None  -> 0.0 

getBalance USD (Map [(DKK, 42.0);(USD, 43.25)])
getBalance DKK (Map [(DKK, 42.0);(USD, 43.25)])
getBalance DKK (Map []);;


let addToBalance (balances: Map<'a, float>) ((currency:'a), (transactionType:TransactionType), (amount:float)) : Map<'a, float> = 
    let currentBalacnce = getBalance currency balances 
    let newbalance = if transactionType = Deposit then currentBalacnce + amount else (currentBalacnce - amount)
    balances.Add(currency, newbalance)

let bal = addToBalance Map.empty (DKK, Deposit, 100.0);; 
addToBalance bal (USD,Withdrawal, 100.0)

let getBalancesOfAccount (account:Account) : Map<Currency, float> = 
    let allTrans = account.transactions 
    let folded = allTrans |> List.fold (fun acc x -> addToBalance acc (x.currency, x.transType,x.amount)) Map.empty
    folded

getBalancesOfAccount exAccount01

let conversionRates =
    Map [(USD, Map [(EUR, 0.93); (DKK, 6.94)]);
         (EUR, Map [(USD, 1.07); (DKK, 7.46)]);
         (DKK, Map [(USD, 0.14); (EUR, 0.13)])]

let getConvRate (fromCurrency: 'a) (toCurrency:'b) (rates:Map<'a,Map<'b,'c>>) : 'c when 'a: comparison and 'b : comparison = 
    match Map.tryFind fromCurrency rates with 
    | Some innerTable -> 
                    match Map.tryFind toCurrency innerTable with 
                    | Some value -> value 
                    | None -> failwith "Cannot find toCurrency"
    | None -> failwith "Cannot find fromCurrency" 
getConvRate USD DKK conversionRates 

List.map (fun x -> if x <> 2 then x+1 else x) [1;2;3]         

let convertFunds (account:Account) (fromCurrency: Currency) (toCurrency: Currency) (rates: Map<Currency, Map<Currency, float>>) : Account = 
        let transactions = account.transactions 
        let rate = getConvRate fromCurrency toCurrency rates
        let newTrans = transactions |> List.map (fun x -> if x.currency = fromCurrency then
                                                            mkTrans (x.transType, toCurrency, x.amount * rate, x.date) 
                                                            else x)
        {
            owner = account.owner
            number = account.number
            transactions = newTrans}

let exAccount03 = convertFunds exAccount01 USD EUR conversionRates


