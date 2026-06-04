// HR Chapter 12.6 - maybe<'a>
// Maybe

// Uses type option<'a>

type optionBuilder() =
  member this.Bind(x,f) =
    match x with
      None -> None
    | Some v -> f v
  member this.Return x = Some x

type optionBuilder2() =
  member this.Bind(x,f) =
    printf "this.Bind: %A\n" x
    match x with
      None -> None
    | Some v -> f v
  member this.Return x =
    printf "this.Return: %A\n" x
    Some x
  
let maybe = optionBuilder()

let maybe = optionBuilder2()

maybe { let x = 56
        let! y = Some 78
        return x+y }

let ex = 
  let x = 56
  maybe.Bind(Some 78, fun y -> maybe.Return(x+y))

maybe { let x = 56
        let! y = Some 78
        let! z = None
        return x+y }

let ex =
  let x = 56
  maybe.Bind(Some 78,
             fun y -> maybe.Bind(None,
                                 fun z -> maybe.Return(x+y)))

// 42 will not be printed because we stop after z.  
maybe { let x = 56
        let! y = Some 78
        let! z = None
        let! v = Some 42
        return x+y+v }
  
let _ =
  let x = 56
  maybe.Bind(Some 78,
             fun y -> maybe.Bind(None,
                                 fun z -> maybe.Bind(Some 42,
                                                     fun v -> maybe.Return(x+y+v))))
// Option building supporting return! and if
type optionBuilder3() =
  member this.Bind(x,f) =
    match x with
      None -> None
    | Some v -> f v
  member this.Return x = Some x
  member this.ReturnFrom m = m
  member this.Zero() = None

let maybe = optionBuilder3();

maybe { let! x = Some 27
        return x+5 }

maybe.Bind(Some 27,
           fun x -> maybe.Return(x+5))

maybe { let! x = Some 27
        if x = 27 then return "Success" }

maybe.Bind(Some 27,
           fun x -> if x = 27 then maybe.Return "Succes" else maybe.Zero())

maybe { let! x = Some 27
        if x = 27 then return! (Some "Success") }

maybe.Bind(Some 27,
           fun x -> if x = 27 then maybe.ReturnFrom (Some "Succes") else maybe.Zero())

maybe { let! x = Some 26
        if x = 27 then return "Success" }

maybe { let! x = Some 26
        if x = 27 then return! (Some "Success") }

