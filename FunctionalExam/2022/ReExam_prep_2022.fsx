
type TrashItem<'a> = 
    Paper of string 
    | Glass of string 
    | Other of 'a 

let item1 = Other ("Shirt", "Clothes") 
let item2 = Paper "Newspaper"
let item3 = Other ("Sneakers", "shoes") 
let item4 = Glass "Wine glass" 


let items = [Paper "Magasine"; Glass "Bottle"; Glass "Jam"; Other ("Beer can", "Aluminium"); Other ("Bag", "Plastic")]

let ppTrashItem (fnPP:'a->string) (item:TrashItem<'a>) : string = 
    