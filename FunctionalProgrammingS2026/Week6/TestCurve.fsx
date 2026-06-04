#load "Curve.fs";;



let display(title: string, pts: (float*float) list) =
    printfn "=== %s ===" title
    List.iter (fun (x,y) -> printfn "  (%.1f, %.1f)" x y) pts

let c = Curve.point (100.0, 100.0) + Curve.point (200.0, 150.0) + Curve.point (250.0, 80.0)
let clst = Curve.toList c
display("My Curve", clst)