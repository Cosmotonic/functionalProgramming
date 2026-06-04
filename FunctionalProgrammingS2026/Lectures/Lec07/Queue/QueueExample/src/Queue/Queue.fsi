// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 7: Modules

// From Section 7.8: Signature file for Queue with ordering and indexing

module Queue
[<Sealed>]
type Queue<'a when 'a : comparison> =
     interface System.IComparable
     member Item : int -> 'a with get
val empty : Queue<'a>
val put   : 'a -> Queue<'a> -> Queue<'a>
val get   : Queue<'a> -> 'a * Queue<'a>
exception EmptyQueue
