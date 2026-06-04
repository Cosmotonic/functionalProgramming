// HR Chapter 12.1 - 12.3 - mySeq

// Cartesian product of the two sequences [1 .. 3] and ['a' .. 'd']
// using the Seq-library.
Seq.collect
 (fun i ->
   Seq.collect
    (fun ch ->
       Seq.singleton (i, ch)) ['a' .. 'd'])
 [1 .. 3]

// More generally, see exercise HR 11.10a
let cartesian xsq ysq =
  Seq.collect
    (fun x -> Seq.collect (fun y ->
                Seq.singleton (x, y)) ysq)
    xsq
cartesian [1 .. 3] ['a' .. 'd']

let f i = seq { for ch in seq ['a' .. 'd'] do
                  yield (i,ch) }
f 1
f 2
f 3

Seq.collect f (seq {1 .. 3})

let cartesian2 xsq ysq =
  seq { for x in xsq do
          for y in ysq do
            yield (x,y) }

cartesian2 [1 .. 3] ['a' .. 'd']

// Building mySeq

type myseq<'a> = seq<'a>

type MySeqBuilder() =
  member this.For(xs, f) = Seq.collect f xs    // for x in sq do ce
  member this.Yield x = Seq.singleton x        // yield
  member this.YieldFrom xs = xs                // yield!
  member this.Zero () = Seq.empty              // Empty container

let mySeq = new MySeqBuilder()

mySeq { for i in [1 .. 3] do
          for ch in ['a' .. 'd'] do
            yield (i,ch) }

let cartesian3 sqx sqy =
  mySeq { for x in sqx do
            for y in sqy do
              yield (x,y) }
cartesian3 [1 .. 3] ['a' .. 'd']

let inner x sqy =
  mySeq { for y in sqy do yield (x,y) }

let inner x sqy =
  Seq.collect
    (fun y -> Seq.singleton (x,y)) sqy

inner 1 ['a' .. 'd']

let cartesian4 sqx sqy =
  Seq.collect (fun x -> inner x sqy) sqx

let cartesian4 sqx sqy =
  mySeq { for x in sqx do yield! inner x sqy }

cartesian4 [1 .. 3] ['a' .. 'd']


// Support for if, by defining Zero.
mySeq { for i in [1 .. 10] do
          if i % 2 = 0 then yield i }

let sift a xs =
  mySeq { for n in xs do
            if n % a <> 0 then yield n }
sift 2 [1;2;3;4]

// List and Array expression

[ for x in [1..3] do yield x*x ]

Seq.toList(seq {for x in [1..3] do yield x*x})

[| for x in [1..3] do yield x*x |]

Seq.toArray (seq {for x in [1..3] do yield x*x})

// mySillySeq

type SillySeqBuilder() =
  member this.Bind(xs, f) = Seq.collect f xs
  member this.Return x = Seq.singleton x

let mySillySeq = new SillySeqBuilder()

mySillySeq {
  let! i = [1 .. 3]
  let! ch = ['a' .. 'd']
  return (i, ch) }

mySeq { for i in [1 .. 3] do
          for ch in ['a' .. 'd'] do
            yield (i,ch) }

            
