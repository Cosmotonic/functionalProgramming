// Declare an F# function noRemainderP m n of type int-> int-> bool that returns true
// if m modulo n is 0, that is m % n is 0. For instance, noRemainderP 10 2 evaluates to true and
// noRemainderP 10 3evaluates to false. The noRemainderP function is not recursive.

let noRemainderP m n = 
    if m % n = 0 then true
    else false 

noRemainderP 10 3

// Declare an F# function checkNumber n m of type int-> int-> bool that evaluates to
// true if noRemainderP m l is true for all 1 ≤ l ≤ n; otherwise checkNumber evaluates
// to false. For instance, checkNumber 10 2520 evaluates to true and checkNumber 11
// 2520 evaluates to false. 
// tag 1 ≤ l ≤ n;  means all numbers from 1 to n. 
// CheckNumber n m tjekker om m kan deles uden rest med alle tal fra 1 til n.
let rec checkNumber n m = 
    if n < 1 then true 
    elif noRemainderP m n then checkNumber( n - 1 ) m  // no remainderP m n retunere true/false så tjekker vi næste nummer. 
    else false 

checkNumber 10 2520 
checkNumber 11 2520 // 2520 % 11 <> 0    hence it returns false and checknubmer will hit else condition and be false. 
checkNumber 4 12 

// Declare a tail–recursive F# function untilTrue f of type (int-> bool)-> int, that repeatedly
// evaluates the function f on the integers n ≥ 1 until the first n is found where f evaluates
// to true. The first n found is the result of the evaluation. For instance, untilTrue (fun n->
// n %3=0)evaluates to 3 because 1 % 3 is 1, 2 % 3 is 2 and 3 % 3 is 0.

// TAG: tail-recursion betyder den arbejder fremad, ikke bagud. Mere som en while loop. 
// untilTrue f tester tallene fra 1 og opad indtil funktionen f returnerer true.
let untilTrue f =
    let rec loop n =
        if f n then n // The first n found is the result of the evaluation.
        else
            loop (n + 1) // increment 
    loop 1

untilTrue (fun n -> n % 3 = 0) // Take a number n and evaluate whether n % 3 = 0. Return true or false. 

// ** Explain why your solution is tail–recursive.
// ANSWER: 
(*
The solution is tail-recursive because the recursive call is the last operation performed in the function.
In the else branch, the function directly returns:
    loop (n + 1)
There is no additional computation after the recursive call returns. 
Therefore, the compiler can optimize the recursion to behave like an iterative loop and avoid growing the call stack.
*)

// ** Explain what happens if you evaluate untilTrue (fun _-> false).
// The condition f n is never true, so the function keeps calling:
//    loop (n + 1) forever.
// This results in an infinite recursion / infinite loop.


// Declare an F# function findSmallest n of type int-> int, that evaluates to the smallest
// number M, that can be evenly divided by the numbers 1,...,n. That is, M can be divided with
// any number between 1 and n without any remainder. For instance findSmallest 3 evaluates
// to 6, because 6 % 0 is 0, 6 % 1 is 0, 6 % 2 is 0, and 6 % 3 is 0. Also, the numbers 1 to 5 are not
// solutions, e.g., 1%2 is 1, 2%3 is 1, 3%2 is 1, 4%3 is 1 and 5%3 is 2.
// Hint: Can be implemented using untilTrue and checkNumber.

// I IKKE-MONGOL SPROG: Hvad tal kan dividers med alt fra 1 til 3 
let findSmallest n =
    untilTrue (fun m -> checkNumber n m)

findSmallest 3
findSmallest 10


// Question 1.2
// Declare an F# function revAppend xs ys of type ’a list-> ’a list-> ’a list that
// appends the reverse of the list xs to the list ys. That is, revAppend [x1; ...; xN] [y1; ...;yM]
// evaluates to the list [xN; ...; x1; y1 ...;yM]. Forinstance, revAppend[1;2][3;4]
// evaluates to [2;1;3;4].

// flyt alt fra første liste omvendt foran ind i anden liste. 
let rec revAppend xs ys = 
    match xs with 
    | [] -> ys
    | x::s -> revAppend s (x::ys)

revAppend [1;2] [3;4]
// revAppend[1;2][3;4] evaluates to [2;1;3;4].

// Declare an F# function average xs of type float list-> float that evaluates to the av
// erage of the values in the list xs. That is, average [x1; ...;xN], evaluates to (x1+···+xN) / N
//  The function average must evaluate to 0.0 in case the list xs is empty. For instance, average
// [1.0;2.0] evaluates to 1.5 and average [] evaluates to 0.0.

let average xs =
    let rec loop xs sum count =
        match xs with
        | [] ->
            if count = 0 then 0.0   // if the list is initially empty return 0.0 
            else sum / float count  // if the list is exhausted but count has incremented divide sum/count

        | x::rest ->
            loop rest (sum + x) (count + 1)
    loop xs 0.0 0

average [3;3;3]


// Declare an F# function maxBy f xs of type
// (’a->’b)->’alist->’a when’b: comparison
// that evaluates to the element xi in the list xs where fxi evaluates to a larger value than any other
// fxj for i ̸ = j and xj also in the list xs. That is, evaluates to an element xi in xs, where fxi ≥ fxj
// for any xj in xs. The function should fail with message “maxBy: empty list” in case xs is empty.
// For instance, maxBy ((+)1) [1..10] evaluates to 10.

let rec maxBy f xs =
    match xs with
    | [] -> failwith "maxBy: empty list"
    | [x] -> x
    | h::t ->
        let bestRest = maxBy f t
        if f h >= f bestRest then h
        else bestRest

maxBy ((+) 1) [1..10]


// Consider the F# function collect f xs of type (’a-> ’b list)-> ’a list-> ’b list.
let rec collect f = function
    []-> []
    | x::xs-> f x @ collect f xs

//The function applies f on each element x in xs and appends each result list. For instance, collect id
// [[1;2];[3;4]] evaluates to [1;2;3;4].

// Explain why the function collect is not a tail–recursive function.

// ANSWER 
// The function first evaluates:  collect f xs
// and then performs:             f x @ ...
// afterwards. Hence the recursive call is not the final operation.

// Declare a tail–recursive version of collect. The function must have same type and effect.
// Hint: The function revAppend may be useful.

let collect1 f xs =
    let rec loop xs acc =
        match xs with
        | [] -> List.rev acc
        | x::rest ->
            loop rest (revAppend (f x) acc)

    loop xs []

collect1 (fun x -> x) [[1;2];[3;4]]

// Question 2 (25%)
// The concept of finite trees is described in HR Chapter 6. A difference list is a list representation where
// the append operation has complexity O(1) and conversion to a single list structure is proportional to the
// length, i.e., O(N). The order of elements in a difference list matters similarly as for F# lists. Consider
// the F# representation of a difference list below

type 'a diffList =
    LISTS of 'a list list
    | SEQ of 'a diffList * 'a diffList

let exDF01 = SEQ (LISTS [[1;2];[3;4]],
                SEQ (LISTS [[];[5]],LISTS [[6;7];[8]]))
let exDF02 = SEQ (LISTS [[1;3];[2;4]],
                SEQ (LISTS [[];[5]],LISTS [[6;7];[8]]))

exDF01 = exDF02

// Declare an F# function mkDiffList of type unit-> ’a diffList that evaluates to an
// empty difference list. For instance: // tag type TYPE list of lists 

let mkDiffList () =
    LISTS []

// Declare an F# function fromList xs of type ’a list-> ’a diffList that tranform the F#
// list xs to a difference list. For instance, fromList [1;2] may evaluate to LISTS [...].

let fromList xs =
    LISTS [xs]

fromList [1;3;4;10]

//  Declare an F# function appendList xs diffList of type ’a list->’adiffList->’adiffList
//  that appends xs to the difference list diffList. Must be of complexity O(1). Show the result of
//  evaluating appendList [1;2] (fromList [3;4])

let appendList xs diffList = 
    SEQ (diffList, fromList xs)

appendList [1;2] (fromList [3;4])

// appends xs to the difference list diffList

//  Declare an F# function append diffXs diffYs of type
//  ’a diffList->’adiffList->’adiffList
//  that appends diffXs with diffYs. Must be of complexity O(1). Show the result of evaluating
//  append (fromList [1;2]) (fromList [3;4])

let append diffXs diffYs = 
    SEQ (diffXs, diffYs)

append (fromList [1;2]) (fromList [3;4])

// Declare an F# function flatten diffXs of type’a diffList->’a list that converts the difference
// list diffXs to an F# list with elements in the same order. For instance, flatten exDF01
// evaluates to [1;2;3;4;5;6;7;8].
// tag: flatten 

let rec flatten diffXs =
    match diffXs with
    | LISTS xs -> List.concat xs // "If this node contains lists, combine them into one list."
    | SEQ (left, right) -> flatten left @ flatten right // "If this node is a sequence of two diffLists, flatten both recursively and append them together."

flatten exDF01

// Declare an F# function map f diffXs of type (’a-> ’b)-> ’a diffList-> ’bdiffList,
// that maps the function f on all the elements in the difference list diffXs. For instance, 

let rec map f diffXs =
    match diffXs with
    | LISTS xs ->
        LISTS (List.map (List.map f) xs) // FØRSTE list.map går over ydre lists, 
                                         // ANDEN list.map går over indre lists og applyer f. 
    | SEQ (left, right) ->
        SEQ (map f left, map f right)

map ((+)1) exDF01


// flatten (map ((+)1) exDF01 ) 
// evaluates to [2;3;4;5;6;7;8;9].

// Question 3 (30%)
// The concept of records and collections like lists and maps are described in HR Chapter 3 and 5. We
// consider a Banking System consisting of accounts, transactions and currencies. The currencies DKK,
// EURand USDare supported by the type Currency. The type TransactionType defines two kinds
// of transactions, Deposit and Withdrawal. A transaction, type Transaction, consists of a date
// formatted as (day, month, year), a transaction type transType, a currency and an amount. An
// account consists of an acount number, an owner and a list of transactions.


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


// Question 3.1 
// Consider the list of transaction data below. The type of the list does not match a list of transactions as
// defined above. Each tuple has the format (transaction type, currency, amount, (day, month, year)).
let exTransData01 = [(Deposit, DKK, 45.5, (1,1,2024));
                    (Withdrawal, DKK, 90.0, (1,2,2024));
                    (Deposit, USD, 10.0, (14,2,2024));
                    (Withdrawal, USD, 5.0, (14,1,2024));
                    (Deposit, EUR, 42.0, (16,2,2024))]

// Declare an F# function mkTrans (transType, currency, amount, (day, month, year)) of type
// TransactionType*Currency*float*(int*int*int)->Transaction,that
// converts the tuple into a transaction as defined above. Example shown below
let mkTrans (transTyp, cur, am, (d, m, y)) = 
    {
        date = (d, m, y)
        transType = transTyp
        currency = cur
        amount = am
    }
let exTrans01 = List.map mkTrans exTransData01

// Declare an F# function createAcc accNum owner transactions of type
// string->string->Transactionlist->Account
// that creates an account. Example shown below.
let createAcc accNum owner transactions = 
    {
        number = accNum
        owner = owner 
        transactions = transactions
    }

let exAccount01 = createAcc "001" "Hans" exTrans01;

// Declare an F# function addTransToAcc account transaction of type
// Account->Transaction->Account
// that adds the transaction transaction to the transactions already in the account account. Example
// shown below.
let addTransToAcc account transaction =
    {
        number = account.number // "tag det gamle account-nummer og behold det"
        owner = account.owner
        transactions = transaction :: account.transactions // "læg den nye transaction foran den gamle liste"   
    }
let exAccount02 = addTransToAcc exAccount01 (mkTrans (Deposit, USD, 10.0, (3,2,2024)))

//  Declare an F# function deposit account currency (day, month, year) amount of type
//  Account->Currency->int*int*int->float->Account
//  that adds a transaction of type Deposit to the transactions already in the account account. An
//  example is shown below.

let deposit account currency (day, month, year) amount = 
    {
        number = account.number
        owner = account.owner
        transactions = mkTrans (Deposit, currency, amount, (day, month, year)) :: account.transactions
    }

deposit exAccount01 DKK (13,2,2024) 42.1;;


// Question 3.2
// An account contains transactions of different currencies and its balance is therefore calculated per cur
// rency. The type Balances maps each currency to an amount.
// type Balances = Map<Currency, float>

// Declare an F# function getBalance currency balances of type
// 'a->Map<'a,float>->floatwhen'a: comparison
// that evaluates to the amount registred in the map balances for the key currency. In case the
// currency is not in the map balances, the amount 0.0 is returned. Two examples are shown below.
// tag: match without type 

let getBalance currency balances = 
    match Map.tryFind currency balances with
    | Some amount -> amount // Some is a buld in func on tryFidn. "If a value was found, call it amount and return it."
    | None -> 0.0

getBalance DKK (Map [(DKK, 42.0);(USD, 43.25)]);;
getBalance USD (Map [(DKK, 42.0);(USD, 43.25)]);;

// Declare an F# function addToBalance balances (currency, transType, amount) of type
// Map<’a,float>-> ’a*TransactionType*float->Map<’a,float>
// that evaluates to a new balance where the transaction represented by the tuple has been added to
// balances. Notice, that Deposit transactions increase the balance and Withdrawal transac
// tions decrease the balance. Two examples are shown below.

let addToBalance balances (currency, transType, amount) = 
    let current = getBalance currency balances
    let newAmount =
        match transType with
        | Deposit -> current + amount
        | Withdrawal -> current - amount

    Map.add currency newAmount balances

let dkkAccount1 = addToBalance Map.empty (DKK, Deposit, 100.0)
let dkkAccount2 = addToBalance dkkAccount1 (DKK, Deposit, 100.0)


// Declare an F# function getBalancesOfAccount account of type
// Account->Map<Currency,float>
// that evaluates to the balances of all the transactions in the account. An example is shown below.

let getBalancesOfAccount account =
    let rec loop trans balances =
        match trans with
        | [] -> balances
        | h::t ->
            let updatedBalances = addToBalance balances (h.currency, h.transType, h.amount) // use add to balance which is add and subtract. 
            loop t updatedBalances // 

    loop account.transactions Map.empty

getBalancesOfAccount exAccount01


// Question 3.3
// Consider the conversion rates between the three currencies supported by the Banking System.
let conversionRates =
    Map [(USD, Map [(EUR, 0.93); (DKK, 6.94)]);
         (EUR, Map [(USD, 1.07); (DKK, 7.46)]);
         (DKK, Map [(USD, 0.14); (EUR, 0.13)])]
// For instance, 1.00 USD is converted to 0.93 EUR and 6.94 DKK

//  Declare an F# function getConvRate fromCurrency toCurrency rates of type
//  'a->'b->Map<'a,Map<'b,'c>>->'cwhen'a: comparisonand’b: comparison
//  that evaluates to the conversion rate going from the currency fromCurrency to the currency toCur
//  rency. The function should fail with a meaningful error message if a conversion rate does not
//  exists. For instance, getConvRate USD DKK conversionRates evaluates to 6.94.

let getConvRate fromCurrency toCurrency rates =
    match Map.tryFind fromCurrency rates with
    | Some innerMap ->
        match Map.tryFind toCurrency innerMap with
        | Some rate -> rate
        | None -> failwith "Conversion rate not found"
    | None -> failwith "From currency not found"

getConvRate USD EUR conversionRates


// Declare an F# function convertFunds account fromCurrency toCurrency rates of type
// Account-> Currency-> Currency-> Map<Currency,Map<Currency,float>>-> Account
// that converts all transactions of currency fromCurrency to transactions of currency toCurrency
// using the conversion rate as given by rates. An example is shown below.

let convertFunds account fromCurrency toCurrency rates =
    let conversionRate =  getConvRate fromCurrency toCurrency rates     // Find the conversion rate between the two currencies
    let convertedTransactions =
        List.map (fun transaction -> 
            if transaction.currency = fromCurrency then // Check if this transaction uses the currency we want to convert from
                {
                    // Keep same date, type, add currency
                    date = transaction.date
                    transType = transaction.transType
                    currency = toCurrency
                    amount = transaction.amount * conversionRate // Multiply amount by conversion rate
                }
            // If currency does not match, keep transaction unchanged
            else transaction
        ) account.transactions
    // Create a NEW account with updated transactions
    {
        number = account.number
        owner = account.owner
        transactions = convertedTransactions
    }

let exAccount03 = convertFunds exAccount01 USD EUR conversionRates


// Question 4.1
// Sequences are described in HR Chapter 11. Fizz buzz is a game used to teach division. Players take turns
// to count incrementally, replacing any number divisible by three with the word "fizz", and any number
// divisible by five with the word "buzz", and any number divisible by both three and five with the word
// "fizzbuzz".

// Declare an F# function fizzBuzz n of type int-> string that evaluates to
let fizzBuzz n =
    if n % 3 = 0 && n % 5 = 0 then
        "fizzbuzz"
    elif n % 3 = 0 then
        "fizz"
    elif n % 5 = 0 then
        "buzz"
    else
        string n

fizzBuzz 15

// Declare an F# sequence fizzBuzzSeq of type string seq, representing the sequence of
// fizzBuzz n for n ≥ 0. For instance, Seq.take 4 fizzBuzzSeq evaluates to the sequence
// seq ["FizzBuzz"; "1";"2";"Fizz"].

let fizzBuzzSeq =
    seq {
        for n in 0 .. System.Int32.MaxValue do 
            yield fizzBuzz n // eg. seq {  yield 1 yield 2 yield 3 } = seq [1;2;3]
    }

Seq.take 4 fizzBuzzSeq // "Only return the first 4 elements from the sequence."

//  Declare an F# sequence fizzBuzzSeq2 of type (int * string) seq, containing all ele
// mentsfromfizzBuzzSeqexceptthosethatdonotmaptoeither"Fizz","Buzz"or"FizzBuzz".
// Each elements is transformed into a pair with first component being the index of the element
// in fizzBuzzSeq and the second component the element itself. For instance Seq.take 4
// fizzBuzzSeq2 evaluates to seq [(0, "FizzBuzz"); (3, "Fizz"); (5, "Buzz");
// (6, "Fizz")]. The pair (0, "FizzBuzz") is in the sequence because fizzBuzz 0 evalu
// ates to "FizzBuzz".
// Hint: The library function Seq.mapi may be useful.

let fizzBuzzSeq2 =
    fizzBuzzSeq
    |> Seq.mapi (fun i x -> (i, x))
    |> Seq.filter (fun (_, x) ->
        x = "fizz" || x = "buzz" || x = "fizzbuzz")

Seq.take 4 fizzBuzzSeq2 // input takes for elements. 