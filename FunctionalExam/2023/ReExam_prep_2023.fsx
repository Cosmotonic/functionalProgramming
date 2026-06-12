
// Question 3.  






// Question 1. 

type Album = Elem list 
and Elem = 
    PicElem of string 
    | AlbumElem of string * Album 

let pic1 = PicElem "Peter.jpg"
let pic2 = PicElem "Gitte.jpg" 
let pic3 = PicElem "Mother.jpg" 
let pic4 = PicElem "Father.jpg" 
let pic5 = PicElem "Hans.jpg" 

let albumElem1 = AlbumElem ("Family", [pic3; pic4])
let albumElem2 = AlbumElem ("School", [pic5])
let albumelem3 = AlbumElem ("All", [pic1; albumElem1; pic2; albumElem2])
let album = [albumelem3]

