// Experiments with F# asynchronous HTTP
// sestoft@itu.dk 2012-04-23
#time
open System;;           // Uri
open System.Net.Http;;  // HttpClient
open System.Xml;;       // XmlDocument, XmlNode

let urls = ["http://www.itu.dk"; "http://www.dmi.dk";
            "http://www.amazon.com"; "http://www.dr.dk";
            "http://www.vg.no"; "http://www.yr.no"; "http://www.google.dk";
            "http://www.politikken.dk"; "http://www.ing.dk"; "http://www.dtu.dk"];;
            
let urls = urls @ urls @ urls  @ urls @ urls 

// string -> int
let lengthSync (url : string) =
    printf ">>>%s>>>\n" url    
    use hc = new HttpClient()
    let html = hc.GetStringAsync(Uri(url)).Result
    printf "<<<%s<<<\n" url
    html.Length

lengthSync("http://www.itu.dk")

[ for url in urls do yield lengthSync url]
// Real: 00:00:10.679, CPU: 00:00:01.273, GC gen0: 0, gen1: 0, gen2: 0

// Naive parallelization:

let lens = 
  let tasks = [ for url in urls do yield async { return lengthSync url } ]
  Async.RunSynchronously(Async.Parallel tasks)
// Real: 00:00:02.046, CPU: 00:00:00.539, GC gen0: 2, gen1: 2, gen2: 2
                                
// string -> Async<int>
let lengthAsync (url : string) = 
    async {
        printf ">>>%s>>>\n" url
        use hc = new HttpClient()
        let! html = Async.AwaitTask (hc.GetStringAsync(Uri(url)))
        printf "<<<%s<<<\n" url
        return html.Length
        }
    
Async.RunSynchronously(lengthAsync("http://www.itu.dk"))

let lens = 
  let tasks = [ for url in urls do yield lengthAsync url]
  Async.RunSynchronously(Async.Parallel tasks)
// Real: 00:00:00.701, CPU: 00:00:00.401, GC gen0: 1, gen1: 1, gen2: 1
  
// ----------------------------------------------------------------------
// These timing examples do not demonstrate much gain from async,
// possibly because the web access is performed on individual threads 
// rather than by the underlying nonblocking IO machinery.

// Wall-clock time in seconds to execute f():
let duration (f : unit -> 'a) : float =
    let t1 = DateTime.Now
    let result = f()
    let t2 = DateTime.Now
    t2.Subtract(t1).TotalMilliseconds / 1000.0

// string -> float
let timeSync (url : string) = 
    let hc = new HttpClient()
    duration(fun () -> hc.GetStringAsync(Uri(url)).Result)

timeSync("http://www.diku.dk")

[ for url in urls do yield timeSync url];;

// string -> Async<float>
let timeAsync (url : string) = 
    async { return timeSync(url) }

Async.RunSynchronously(timeAsync("http://www.diku.dk"))

Async.RunSynchronously(Async.Parallel [ for url in urls do yield timeAsync url])

// ----------------------------------------------------------------------
// NCBI examples in F# (from C# Precisely 2nd ed chapter 23)
// File cs/ex-async-protein.cs

let server = "http://www.ncbi.nlm.nih.gov/entrez/eutils/"

// NcbiEntrezAsync : string -> Async<string>
let NcbiEntrezAsync(query : string) =
    async {
        let hc = new HttpClient()
        let url = server + query
        let! response = Async.AwaitTask (hc.GetStringAsync(Uri(url)))
        return response
        }

// val NcbiProteinAsync : string -> Async<string>
let NcbiProteinAsync(id : string) =
    NcbiEntrezAsync("efetch.fcgi?rettype=fasta&retmode=text&db=protein&id=" + id)


// NcbiProteinParallelAsync : string list -> Async<string []>
let NcbiProteinParallelAsync(ids : string list) =
    async {
        let tasks = [ for id in ids do yield NcbiProteinAsync(id) ]
        return! Async.Parallel(tasks);
        }

let prots = ["P01308"; "P01315"; "P01317"]

Async.RunSynchronously(NcbiProteinParallelAsync(prots))

