# General feedback for assignment 2

### Function types

Functions have types, too.

`f : string * int -> string` is a function that takes a tuple containing a string and an integer, and returns an integer. The type of `f` is `string * int -> string`. The type of `f ("echo", 3)` is `string`.

`g : string -> int -> string` is a function that takes first a string, then an integer, and returns a string. The type of `g` is `string -> int -> string`. The type of `g "echo" 3` is `string`.

`h : (int -> int) -> int` is a function that takes another function (of type `int -> int`) as input and returns an integer. The type of `h` is `(int -> int) -> int`. The type of `h (fun x -> x+x)` is `int`.

Make sure you write your functions so that they match the type that is asked for in the question. Some of you wrote `dupn` in exercise 2.2 as a function that takes a single tuple argument rather than a function that takes two separate arguments.

#### Type inference

In exercise 2.1, you are asked to write a function `dup : string -> string` that concatenates a string with itself.

If you simply write `let dup s = s + s`, the function will work - but the type checker in F# will not be able to infer that `s` is a string. For all F# knows, `s` might be an integer. But there are several ways to tell F# that your function `dup` does in fact have the type `string -> string`. Here are a few:

```fsharp
// Specify the type of s directly
let dup (s: string) =
    s + s

// Use a library function on strings
let dup s =
    String.replicate 2 s

// Use ^ (which will get you a compiler warning, because it only exists for compatibility with e.g. OCaml)
let dup s =
    s ^ s
```

### What base case should I choose?

In exercise 2.2 (`dupn`) and 2.5 (`pow`), some of you choose `n = 1` as base case and return the input string `s`. Although this approach computes the correct result for `n >= 1`, it doesn't cover the `n = 0` case. 

Generally, when choosing base cases for recursive functions, we usually choose the base case such that we can return the identity element/neutral element of the computed value. The identity element is `0` for addition/subtraction, `1` for multiplication/division, `""` for strings (the empty string), `[]` for lists (the empty list) and so on.

### `when` guards

In exercise 2.6 (Pascal's Triangle), we want to declare a function `bin: int * int -> int` that computes binomial coefficients. On p. 40 in the book, we get a definition of the $k$ -th binomial coefficient of the $n$ -th row, $\binom{n}{k}$, for $0 \le k \le n$:

$$
    \binom{n}{0} = \binom{n}{n} = 1 
$$

and

$$
    \binom{n}{k} = \binom{n-1}{k-1} + \binom{n-1}{k} \quad \text{if } n \neq 0, k \neq 0, \text{ and } n > k
$$

Note the restrictions on $n$ and $k$. The recursive case in our `bin` function needs to account for these restrictions, i.e. $n \neq 0$, $k \neq 0$, and $n > k$. How do we do this? `when` guards! `when` guards act like filters in pattern-matching:

```fsharp
let rec bin (n, k) =
    match k with
    | 0 -> 1
    | _ when k = n -> 1
    | _ when n > k -> bin (n-1, k-1) + bin (n-1, k)
```

Or, with a more defensive approach:
```fsharp
let rec bin (n, k) =
    if k < 0 || k > n then failwith "input (n,k) must satisfy n >= 0, k >= 0, and n > k."
    match k with
    | 0 -> 1
    | k when k = n -> 1
    | _ -> bin (n-1) (k-1) + bin (n-1) k
```

### Mathematical meaning

When exercise 2.7 asks for the mathematical meaning of `f (x,y)`, you need to provide a mathematical expression for what `f` computes in relation to `x` and `y`.

For example, the mathematical meaning of `fact x` (the function on p. 8 of the book) is $x!$. The mathematical meaning of `f (x, y)` in exercise 2.7 is $x! \cdot y$. 

### For which arguments does a recursive function terminate?

When exercise 2.7 asks for the inputs for which the evaluation of the recursive function `f` terminates, the answer is not simply "when `x = 0`". Instead, *because* the base case is `x = 0`, the recursive function `f` is guaranteed to terminate eventually for all integer tuples `(x,y)` where `x >= 0`. If `x < 0`, we will never reach the base case, and so the evaluation of `f` will never terminate.
