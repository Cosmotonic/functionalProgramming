module Curve
type Curve =  C of (float*float) * ((float*float) list)
let map  f (C(p0,ps)) = C(f p0,List.map f ps)
let mapP g (C(p0,ps)) = C(p0,List.map (g p0) ps)
type Curve with
  static member (+) (c1:Curve, c2:Curve) =
    match (c1,c2) with
    | (C(p1,ps1),C(p2,ps2)) -> C(p1,ps1@(p2::ps2))
  static member (*) (a: float, c: Curve) =
    let multA  (x0,y0) (x,y) =
      (x0 + a * (x - x0), y0 + a * (y - y0))
    mapP multA c
  static member (|^) (c:Curve, ang: float) =
    let piFact = System.Math.PI / 180.0
    let cs = cos (piFact * ang)
    let sn = sin (piFact * ang)
    let rot (x0,y0) (x,y) =
      let (dx,dy) = (x - x0,y - y0)
      (x0 + cs * dx - sn * dy, y0 + sn * dx + cs * dy)
    mapP rot c
  static member (|^) (c:Curve, ang: int) = c |^ (float ang)
  static member (-->) (c: Curve, (x1,y1): float*float) =
    match c with
    | C((x0,y0),_) -> map (fun (x,y) -> (x-x0+x1, y-y0+y1)) c
  static member (><) (c:Curve, a: float) =
    map (fun (x,y) -> (2.0 * a - x, y)) c

let point (p: float*float) = C (p,[])
let verticRefl (c:Curve) (b:float) =
    map (fun (x,y) -> (x, 2.0*b - y)) c
let boundingBox (C((x0,y0),ps)) =
    let minmax ((minX,minY),(maxX,maxY)) ((x,y):float*float) =
        ((min minX x, min minY y),(max maxX x, max maxY y))
    List.fold minmax ((x0,y0),(x0,y0)) ps
let width (c:Curve)  = let ((minX,_),(maxX,_)) = boundingBox c
                       maxX - minX
let height (c:Curve) = let ((_,minY),(_,maxY)) = boundingBox c
                       maxY - minY
let toList (C(p,ps)) : (float*float) list = p :: ps