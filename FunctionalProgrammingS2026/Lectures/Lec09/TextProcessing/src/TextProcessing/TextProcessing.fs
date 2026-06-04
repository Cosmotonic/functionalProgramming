// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 10: Asynchronous and parallel computations. 
// Appendix B: The TextProcessing module: implementation file 
// 2024-04-02 NHA: Removed saveValue and restoreValue as depricated.

module TextProcessing 

open System.Text.RegularExpressions

let captureSingle (ma:Match) (n:int) =
   ma.Groups.[n].Captures.[0].Value

let captureList (ma:Match) (n:int) = 
  let capt = ma.Groups.[n].Captures
  let m = capt.Count - 1 
  [for i in 0..m -> capt.[i].Value]

let captureCount (ma:Match) (n:int) = 
  ma.Groups.[n].Captures.Count

let captureCountList (ma:Match) = 
  let m = ma.Groups.Count - 1
  [for n in 0..m -> ma.Groups.[n].Captures.Count]

open System
open System.IO

let fileXfold f e0 path =
  use s = File.OpenText path 
  let rec fld e =   
    if s.EndOfStream then e 
    else fld (f e s) 
  let res = fld e0 
  s.Close() 
  res

let fileXiter g path = 
  use s = File.OpenText path 
  while not(s.EndOfStream) 
    do g s 
  s.Close()

let fileFold f e s = 
  fileXfold (fun e s -> f e (s.ReadLine())) e s

let fileIter g s = 
  fileXiter (fun s -> g (s.ReadLine())) s

(* NH 2024-04-02: Depricated
open System.IO
open System.Runtime.Serialization.Formatters.Binary

let saveValue v path = 
  let fsOut = new FileStream(path,FileMode.Create) 
  let formatter = new BinaryFormatter() 
  formatter.Serialize(fsOut,box v) 
  fsOut.Close()

let restoreValue path =  
  let fsIn = new FileStream(path,FileMode.Open) 
  let formatter = new BinaryFormatter() 
  let res = formatter.Deserialize(fsIn) 
  fsIn.Close() 
  unbox res
*)

open System.Globalization
open System

exception StringOrderingMismatch

[<CustomEquality;CustomComparison>]
type orderString = 
  {Str: string; Cult: string; Cmp: string->string->int} 
  override s.ToString() = s.Str 
  interface System.IComparable with
    member s1.CompareTo sobj = 
      match sobj with
      | :? orderString as s2 -> 
        if s1.Cult <> s2.Cult then raise StringOrderingMismatch
        else
        match s1.Cmp s1.Str s2.Str with
        | 0 -> compare s1.Str s2.Str
        | z -> z 
      | _ -> 
        invalidArg "sobj" 
                   "cannot compare values with different types"
  override s1.Equals sobj = 
    match sobj with
    | :? orderString as s2 -> s1 =  s2 
    | _                    -> false
  override s.GetHashCode() = hash(s.Str)

let orderString (cult: string) = 
  let culInfo = CultureInfo cult
  let comp s1 s2 = 
    String.Compare(s1,s2,culInfo,CompareOptions.None)
  fun s -> {Str = s; Cult = cult; Cmp = comp}: orderString

let orderCulture s = s.Cult
