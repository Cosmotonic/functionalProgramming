// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 10: Asynchronous and parallel computations. 
// Appendix B: The TextProcessing module: signature file
// 2024-04-02 NHA: Removed saveValue and restoreValue as depricated.

module TextProcessing 

open System.Text.RegularExpressions

val captureSingle : Match -> int -> string

val captureList : Match -> int -> string list

val captureCount : Match -> int -> int

val captureCountList : Match -> int list

open System.IO

val fileXfold : ('a -> StreamReader -> 'a) -> 'a -> string -> 'a

val fileXiter : (StreamReader -> unit) -> string -> unit

val fileFold : ('a -> string -> 'a) -> 'a -> string -> 'a

val fileIter : (string -> unit) -> string -> unit

(* NH 2024-04-02: Depricated
open System.IO
open System.Runtime.Serialization.Formatters.Binary

val saveValue:     'a -> string -> unit
val restoreValue:  string -> 'a
*)

open System
open System.Globalization

exception StringOrderingMismatch

[<Sealed>]
type orderString =
  interface IComparable

val orderString : string -> (string -> orderString)

val orderCulture : orderString -> string
