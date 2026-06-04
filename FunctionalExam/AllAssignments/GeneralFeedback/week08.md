# General feedback, assignment 8

Great job on this week's assignment!

## Fibonacci with while loop

Some of you forgot the case when `n = 0`. The recurring example was that the while loop would only initiate if `n > 0`, which would never happen if the given argument was `n = 0`. Or that two mutable variables were initially set to `prev1 = 0` and `prev2 = 1`, but only `prev2` would ever be returned (even if the while loop was run when `n = 0`).

Here's one way to write the function to compute Fibonacci numbers in an imperative style:

```fsharp
let impFib n =

    // Create mutable variables for previous Fib numbers, and a counter
    let mutable prev1 = 0
    let mutable prev2 = 1
    let mutable counter = 0

    // Compute every Fibonacci number up until the n+1'th Fibonacci number
    while counter < n do
        let t = prev1 + prev2 // compute next Fibonacci number
        prev1 <- prev2 // update prev1
        prev2 <- t
        counter <- counter + 1

    prev1 // return the n'th Fibonacci number
```

## Continuations

Continuations are difficult to understand! For me personally, the best way to *try* to understand is to trace through functions that use continuations, with simple/small arguments. Here's an example with the factorial function:

```fsharp
let factC n =
    let rec aux m c =
        match m with
        | 0 -> c 1
        | m' -> aux (m' - 1) (fun res -> c (m' * res))
    aux n id
```

By the way, `id` is the identity function - it returns the same value as its input, i.e. `fun x -> x`, so `id 2` evaluates to `2`. You can use it by just writing `id <argument>`. `id` is often passed as the initial continuation function.

Anyway, here's the trace of `factC 4` - we expect the result to be $4 \cdot 3 \cdot 2 \cdot 1 = 24$:
```fsharp
// factC 4 calls the inner function `aux`
aux 4 (fun x -> x)
    aux (4-1) (fun res -> (fun x -> x) (4 * res)) // replace variable names with values
    aux 3 (fun res -> 4 * res) // simplify
        aux (3-1) (fun res' -> (fun res -> 4 * res) (3 * res'))
        aux 2 (fun res' -> 4 * (3 * res'))
            aux (2-1) (fun res'' -> (fun res' -> 4 * (3 * res')) (2 * res''))
            aux (1) (fun res'' -> 4 * (3 * (2 * res'')))
                aux (1-1) (fun res''' -> (fun res'' -> 4 * (3 * (2 * res''))) (1 * res'''))
                aux (0) (fun res''' -> 4 * (3 * (2 * (1 * res'''))))
                // Now m = 0, so we hit the base case and call `c 1`:
                    (fun res''' -> 4 * (3 * (2 * (1 * res''')))) 1
                    4 * (3 * (2 * (1 * 1)))
                    4 * (3 * (2 * 1))
                    4 * (3 * 2)
                    4 * 6
                    24       
```

For continuations, there is a certain pattern that you can use in most, if not all, cases:

1. For the base case: The continuation function is completed. It now sits on the heap as a chain of function calls. The first calling the second and so on. Here we need to call the continuation with some initial value.
2. For the recursive call: Create an anonymous function which receives a value and uses it to call `c`.
   Looking at the definition of foldBack, the idea is that in order to compute the intermediary value for some element on index `x` we need the result of calling `f` on the next element on index `x + 1`. We do not have this yet, but we will and we call this value `res`. The continuation function can be seen as a contract. We need some value which we don't have. But once we get it we know how we should use it, in this case for `f x res`.
3. In most cases we can use `id` to initiate the continuation. `id` stands for `identity` and has the definition `fun x -> x`. It's a function which merely returns whatever it is given. In this case, if the list is empty the result should be equal to the initial accumulator so it fits perfectly.

## Why do we even need tail-recursion?

Take the basic factorial function, which might look something like the following in F# (we ignore when `n < 0` for
simplicity's sake):

```F#
let rec factorial (n : int) : int =
    match n with
    | 0 -> 1
    | m -> m * (factorial (m - 1))
```

If we try to run this definition of the factorial function on a reasonably large number, say 1 million, we get something like the following:

```sh
> factorial 1000000;;
Stack overflow.
Repeated 261422 times:
...
```

Because of the way we defined our function, every time we call the function, we need to wait for the recursive call to finish, before we can finish ourselves. This means that we fill the stack with `n` function calls when calling the
function with `n`.

What can we do about this? Well, if we don't have to do anything with the result of the recursive call
other than just returning it, then we don't need to keep track of all our calls, as we can just return our final result from when the function hits the base case. This is what we call *tail-recursion* - making sure that the last thing that happens in our function is the recursive call (which we just return).

One thing to note is that using continuations to write functions tail-recursively reduces the stack size - but not necessarily the amount of work that needs to be done. Thus, computing Fibonacci numbers with `fibC` will have exponential(!) running time - but at least you won't get issues with stack overflow. :)