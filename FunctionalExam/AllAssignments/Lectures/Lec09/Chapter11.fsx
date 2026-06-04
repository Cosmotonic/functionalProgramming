// Mutable records

type counter = {mutable count : int;
                name : string}

let mkCounter n = {count = 0; name = n}
let incr c = c.count <- c.count+1
let c1 = mkCounter "c1"
let c2 = mkCounter "c2"
incr c1
incr c1
incr c2
incr c2
incr c1
incr c2
incr c1

type counter2 = {count2 : int;
                 name2 : string}

let mkCounter2 n = {count2 = 0; name2 = n}
let incr2 c = {count2 = c.count2+1; name2=c.name2}

let c1 = mkCounter2 "c1"
let c2 = mkCounter2 "c2"
incr2 c1
incr2 c1
incr2 c2
incr2 c2
                                           
let mutable c1 = mkCounter2 "c1"
let mutable c2 = mkCounter2 "c2"
c1 <- incr2 c1;c1
c1 <- incr2 c1;c1
c2 <- incr2 c2;c2
c2 <- incr2 c2;c2
c1 <- incr2 c1;c1
c2 <- incr2 c2;c2
c1 <- incr2 c1;c1


// Slide 17
fun () -> 3+4

// Slide 18
let idWithPrint i = let _ = printfn "%d" i
                    i
idWithPrint 3
                   
fun () -> (idWithPrint 3) + (idWithPrint 4)
it()

// Slide 19
seq [10; 7; -25]

let nat = Seq.initInfinite (fun i -> i)
let nat = Seq.initInfinite idWithPrint
Seq.item 4 nat

// Slide 20
let even = Seq.filter (fun n -> n%2=0) nat
Seq.item 9 even
Seq.toList (Seq.take 4 even)

// Slide 21
// Section 11.4 - Sieve of Eratosthenes

// Slide 22
let sift a sq = Seq.filter (fun n -> n % a <> 0) sq

let rec sieve sq =
  Seq.delay (fun () -> 
               let p = Seq.item 0 sq
               Seq.append
                (Seq.singleton p)
                (sieve (sift p (Seq.skip 1 sq))))

let primes = sieve (Seq.initInfinite (fun n -> n+2))
let nthPrime n = Seq.item n primes
#time
nthPrime 300
nthPrime 900

// Slide 23
let primesCached = Seq.cache primes
let nthPrime' n = Seq.item n primesCached
nthPrime' 300
nthPrime' 900

// Slide 24
let rec sieve sq =
  seq { let p = Seq.item 0 sq
        yield p
        yield! sieve (sift p (Seq.skip 1 sq)) }

let primes = sieve (Seq.initInfinite (fun n -> n+2))
let nthPrime n = Seq.item n primes
nthPrime 500

let primesCached = Seq.cache primes
let nthPrime' n = Seq.item n primesCached
nthPrime' 700

// Slide 25
// Search in directory, page 264
open System.IO
let rec allFiles dir =
  seq {yield! Directory.GetFiles dir
       yield! Seq.collect allFiles (Directory.GetDirectories dir)}
Directory.SetCurrentDirectory @"./FSharpHR08"
let files = allFiles "."
Seq.item 10 files

// Slide 26+27
open System.Text.RegularExpressions
open System.Globalization
#r @"./TextProcessing/src/TextProcessing/bin/Debug/net8.0/TextProcessing.dll"
open TextProcessing

let rec searchFiles files exts =
  let reExts = List.foldBack (fun ext re -> ext+"|"+re) exts ""
  let re = Regex (@"\G(\S*/)([^/]+)\.(" + reExts + ")$")
  seq {for fn in files do
         let m = re.Match fn
         if m.Success 
         then let path = captureSingle m 1
              let name = captureSingle m 2
              let ext  = captureSingle m 3
              yield (path,name,ext)}
                    
let funFiles = Seq.cache (searchFiles (allFiles ".") ["tex";"pdf";"sty"])
Seq.item 0 funFiles
Seq.take 10 funFiles


                   
