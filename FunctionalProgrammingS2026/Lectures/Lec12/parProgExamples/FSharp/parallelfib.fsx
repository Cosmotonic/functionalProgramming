// Computing slowfib(45) is CPU-intensive, ca 2 seconds on Mac M3 Pro
#time
let rec slowfib n = if n<2 then 1.0 else slowfib(n-1) + slowfib(n-2)

// Don Syme examples, adapted
// Times are Mono 6.12.0 on MacOS 12.6.4 2,5 GHz Quad-Core Intel Core i7

let fib42 = slowfib(45)
// Real: 00:00:02.590, CPU: 00:00:00.063, GC gen0: 0, gen1: 0, gen2: 0

let fibs = [ slowfib(45); slowfib(45) ]
// Real: 00:00:05.129, CPU: 00:00:00.124, GC gen0: 0, gen1: 0, gen2: 0

let fibs =
  let tasks = [ async { return slowfib(45) };
                async { return slowfib(45) } ]
  Async.RunSynchronously (Async.Parallel tasks)
// Real: 00:00:02.596, CPU: 00:00:00.126, GC gen0: 0, gen1: 0, gen2: 0

let fibs = [ for i in 0..45 do yield slowfib(i) ]
// Real: 00:00:06.297, CPU: 00:00:06.323

let fibs =
    let tasks = [ for i in 0..45 do yield async { return slowfib(i) } ]
    Async.RunSynchronously (Async.Parallel tasks)
// Real: 00:00:02.594, CPU: 00:00:07.391

// Dissection of this:

// async { return slowfib(i) }
// has type: Async<float>
// an asynchronous task that, when run, will produce a float

// let tasks = [ for i in 0..42 do yield async { return slowfib(i) } ]
// has type: Async<float> list
// a list of asynchronous tasks, each of which, when run, will produce a float

// Async.Parallel tasks
// has type: Async<float []>
// an asynchronous tasks that, when run, will produce a list of floats

// Async.RunSynchronously (Async.Parallel tasks)
// has type: float []
// a list of floating-point numbers
