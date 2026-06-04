
// slide 37
// does color a have color? No Assign it a color 
// does b have a color, no. Is it neighbour to a? Yes, give it another color
// always figure out if hte 

// make the list of country pairs (all countires a country neighbours, but only unique. A, B then not B, A)


type shape =
    Circle of float
    | Square of float
    | Triangle of float*float*float;;

let area = function
    | Circle r
    | Square a-> System.Math.PI * r * r-> a * a
    | Triangle(a,b,c)->
    let s = (a + b + c)/2.0
    sqrt(s*(s-a)*(s-b)*(s-c)) ;;
> val area : shape-> real




let rec map f = function
    | []-> []
    | x::xs-> f x :: map f xs;;

    