// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 6: Modules

// From Section 7.3 and 7.4: Implementation file for vector module with type augmentation and extension 

module Vector
type Vector =
  | V of float * float
  override v.ToString() = match v with V(x,y) -> string(x,y)
let make(x,y)     = V(x,y)
let coord(V(x,y)) = (x,y)
type Vector with
     static member (~-) (V(x,y))           = V(-x,-y)
     static member (+) (V(x1,y1),V(x2,y2)) = V(x1+x2,y1+y2)
     static member (-) (V(x1,y1),V(x2,y2)) = V(x1-x2,y1-y2)
     static member (*) (a, V(x,y))         = V(a*x,a*y)
     static member (*) (V(x1,y1),V(x2,y2)) = x1*x2 + y1*y2
let norm(V(x,y))  = sqrt(x*x + y*y)
