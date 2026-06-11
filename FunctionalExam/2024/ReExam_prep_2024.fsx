

type 'a diffList = 
    LISTS of 'a list list 
    | SEQ of 'a diffList * 'a diffList 


let exDF01 = SEQ (LISTS [[1;2];[3;4]], 
                SEQ (LISTS [[];[5]], LISTS [[6;7];[8]]))

let EXdf02 = SEQ (LISTS [[1;2;3];[]], SEQ (LISTS [[];[]], SEQ (LISTS [[6;7]],LISTS [[8];[]]))) 

let mkDiffList : 'a diffList = SEQ(LISTS[[]], LISTS[[]])

let fromLists (xs:'a list) : 'a diffList = 
    LISTS [xs;[]] 

fromLists [1;2]

let rec appendList (xs:'a list) (diffList: 'a diffList) : 'a diffList = 
    match diffList with 
    | LISTS [a;b] -> LISTS [xs @ a; b] 
    | SEQ (diffA, diffB) -> 
                    let addToSeq = appendList xs diffA
                    SEQ (addToSeq, diffB) 
    | _ -> LISTS [xs] 

appendList [1;2] EXdf02 // (fromLists [3;4])
appendList [1;2] (fromLists [3;4])


let append (diffXs:'a diffList) (diffYs:'a diffList) : 'a diffList = 
    