

// Define a MySeqBuilder class with For and Yield:
type MySeqBuilder() =
        member this.For(xs, f) = Seq.collect f xs
        member this.Yield y = Seq.singleton y

// Make an object of that class:
let mySeq = new MySeqBuilder()
// The object can now build a computation expression:
mySeq { for i in [1 .. 3] do
        for ch in ['a' .. 'd'] do
        yield (i, ch) }


type optionBuilder3() =
    member this.Bind(x,f) =
        match x with
            None -> None
            | Some v-> f v
    member this.Return x = Some x
    member this.Return From m = m
    member this.Zero() = None

let maybe= optionBuilder3()

maybe { let! x= Some 27
        if x = 27 then return "Success" }