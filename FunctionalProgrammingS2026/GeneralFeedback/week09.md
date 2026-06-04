# General feedback for Assignment 9:

## 9.3

When given an assignment like 9.3, which asks you to analyze a problem, it is important that you include your analysis as a comment. Simply having ``bigListK 300000 id co`` is not sufficient.

Additionally, many of you seemed to miss the main point of the exercise. Simply having a large chain of nested functions, does not necessarily cause a stack overflow. In this assignment, calling ``1::k(res)`` recursively, results in a large ``::`` structure, which can first be resolved after the innermost recursion is resolved.


## 9.4
We saw many great more or less tail-recurseive solutions to `leftTree` and `rightTree` generator functions.
When asked to do a tail-recursive function you should think of accumulation or continuation right away. Here are to different version of a `leftTree` function - one using an accumulator (where the tree itself is the accumulating parameter) and one using
continuation.

```fsharp
let leftTreeA n =
    let rec helper m acc = 
        match m with
        | m when m < 0 -> acc
        | _ -> helper (m-1) Node(acc, m, Leaf)
    helper n Leaf


let rec leftTreeC n c =
  if n < 0 then c Leaf
           else leftTreeC (n-1) (fun t -> c (Node(t,n,Leaf)))
```

The point of the rest of this exercise is to compare the different counting function, and you will see that the performance is 
different when you count a left unbalanced tree and a right unbalanced tree, even though the amount of nodes are the same. 


### Benchmarking and comparing the performance of functions

Generally, when asked to compare two or more functions, it's important to include the elapsed time when evaluating the functions on your machine. Timing functions in F# is generally achieved by running the `#time` directive in the interactive terminal. After the directive is run, whenever you evaluate a function call, the elapsed time will be displayed. When an assignment asks you to compare functions you should time those functions and include the results as comments.

Furthermore, it's important to use somewhat large values when comparing functions in this way. If you only test your functions with small values, you won't see any difference in the running time of your implementations; your recorded time is instead dominated by other factors independent of the function you're running.

## 9.6

Most managed to correctly define the sequence of factorial numbers. One way of doing this to derive the sequence of factorial numbers from the sequence of natural numbers:

```fsharp
// assuming a factorial function 'fact' exists
let fac = Seq.initInfinite fact
```

While this is completely correct, it is not the most performant way to generate sequence, as every element of the sequence will be computed by separate calls to a recursive function. Instead it is preferable that the sequence itself is built based on the value of the previous element in the sequence. One way of achieving this is through the use of `Seq.unfold`, which maintains a state throughout the generation of the sequence.

```fsharp
let fac = Seq.unfold (fun (i, prev) -> let next = (i * prev) in Some (next, (i + 1, next))) (1, 1)
```

Another way to achieve this is to use sequence expressions

```fsharp
let fac2 = 
    let rec fac' (n,prev) =
        seq { yield n*prev
              yield! fac'(n+1,n*prev) }
    seq { yield 1;
          yield! fac'(1,1) }
```

It's worthing checking out [Microsoft's guide to sequences](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/sequences), which includes more examples of how to work with them, including working with `Seq.unfold`.

