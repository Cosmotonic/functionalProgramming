// 1 
type TwoLists = {listX: int list;
                 listY: int list}

let rec f {listX = lX; listY = lY} =
    match (lX, lY) with
    | ([], []) -> f {listX=[]; listY=[]}
    | ([], y::ys) -> f {listX=[y]; listY = ys}
    | (x::xs, []) -> {listX=x::xs; listY = []}
    | (xs, y::ys) -> f {listX=y::xs; listY = ys}

// its a record. 
// first match keep infinit loop 
// it moves items from y to x one at a time. 

let rec f2 {listX = lX; listY = lY} =
    match (lX, lY) with
    | ([], [])    ->  {listX=[]; listY=[]}
    | ([], y::ys) -> f {listX=[y]; listY = ys}
    | (x::xs, []) -> {listX=x::xs; listY = []}
    | (xs, y::ys) -> f {listX=y::xs; listY = ys}

let rec f3 {listX =lx; listY = ly} = 
    match (lx, ly) with 
    | (x, []) -> {listX= x; listY = []}
    | (xs, y::ys) -> f3 {listX=y::xs; listY = ys}

let infSeq = Seq.initInfinite id 

let mySeq = seq {
            for n in infSeq do 
                for (n) in infSeq do 
                    (n, n*(-1))
            }

let bool = Seq.forall (fun (x,y) -> x+y=0) (Seq.take 1000 mySeq) 

// 2 
type Album = Elem list
and Elem =
    | PicElem of string
    | AlbumElem of string * Album

let pic1 = PicElem "Peter.jpg"
let pic2 = PicElem "Gitte.jpg"
let pic3 = PicElem "Mother.jpg"
let pic4 = PicElem "Farther.jpg"
let pic5 = PicElem "Hans.jpg"
let albumElem1 = AlbumElem ("Familiy", [pic3;pic4])
let albumElem2 = AlbumElem ("School", [pic5])
let albumElem3 = AlbumElem ("All", [pic1;albumElem1;pic2;albumElem2])
let album1 = [albumElem3]

// Yes 
// no 
// Yes
// No 

let pic6 = PicElem "Kim.jpg"
let pic7 = PicElem "Per.jpg"
let pic8 = PicElem "Ib.jpg"
let pic9 = PicElem "Ane.jpg"
let pic10 = PicElem "CoachAnna.jpg"
let pic11 = PicElem "CoachEmil.jpg"
let albumElem4 = AlbumElem ("TeamA", [pic6;pic7])
let albumElem5 = AlbumElem ("TeamB", [pic8;pic9])
let albumElem6 = AlbumElem ("All", [albumElem4;pic10;albumElem5;pic11])
let BasketTeams = [albumElem6]

// monnomorpic as the types are defined. 
let isAlbumElem (elem:Elem) : bool = 
    match elem with 
    | PicElem x -> false 
    | AlbumElem (x,y) -> true 

isAlbumElem albumElem3
isAlbumElem pic3

let newL i = System.Environment.NewLine+"".PadLeft(i, ' ')

let rec ppAlbum (album:Elem list) : string = 
    match album with 
    | [] -> " "
    | h::tail -> 
        match h with 
        | PicElem e -> "Pic: " + e + newL 4 + ppAlbum tail 
        | AlbumElem (x,y) -> "Album: " + x + newL 4 + ppAlbum y +  ppAlbum tail
printf "%s" (ppAlbum album1)

let rec contentAlbum (album:Elem list) : int * int = 
    album |> List.fold (fun (albs, eles) elem ->
                            match elem with 
                            | PicElem _-> (albs, eles+1) 
                            | AlbumElem (_,subAlbum) -> 
                                    let (subAlb, subEle) = contentAlbum subAlbum
                                    (albs + 1 + subAlb, eles+subEle)) (0, 0)
contentAlbum album1


let countAlbum2 (album:Elem list) : string list =   
    