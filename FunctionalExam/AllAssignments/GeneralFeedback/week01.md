# General feedback for Assignment 1:

## Recursion Formulas
Many submissions skipped stating the recursion formula, indicating a need to clarify the concept behind it. Please refer to Section 1.4 of the book for further details.

**What is a recursion formula?**\
A recursion formula is the mathematical recursive definition of a function. It serves as a blueprint for implementing a recursive function in code by precisely specifying the function's behavior. 

A recursion formula consists of the following components: 
1) A base case, which defines the termination condition of the recursion, i.e., the input for which the recursion stops. For example, for a function computing the factorial of a non-negative integer 𝑛, the base case is:
```math 
0! = 1
```

2) One or more recursive cases, which define the function in terms of smaller inputs, thereby reducing the problem size. For the factorial function, the recursive case is:
```math 
𝑛! = 𝑛 ⋅ ( 𝑛 − 1 )! \quad \text{for} \quad 𝑛 > 0
``` 
Here, (𝑛 − 1)! represents a reduction of the original problem 𝑛! to a strictly smaller subproblem. 

**Why do we need a recursion formula?**\
 Defining a recursion formula helps to:
  1) clearly specify the meaning of the function independently of its implementation, 
  2) ensure that the recursion terminates, 
  3) and guide the correct implementation of the corresponding recursive function.



## The "sum"-function
A few had issues with the “sum” function (Ex 1.7). Be sure to read the read how it should work in the exercises, and maybe do the math on paper if you’re not sure.
Here is the description from the book:
sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)

Here is the correct code:
```fsharp
let rec sum (m, n) = 
    match n with
    | 0 -> m
    | n -> m + n  + sum(m, (n-1))
```
## Misssing evaluation
 Many of you skipped evaluating the recursive functions in 1.5 and 1.6 as asked for. A fully recursive evaluation of f(4) in 1.5 should look like this:
```
// f 4

// 4 + (f 3)

// 4 + 3 + (f 2)

// 4 + 3 + 2 + (f 1)

// 4 + 3 + 2 + 1 + (f 0)

// 4 + 3 + 2 + 1 + 0

// 10
```
It may seem trivial to do, but it’s good practice, and a nice tool to use, once your recursive functions get more juicy - which they will.

## Environments
The environment asked for in 1.9 is not just a bunch of let expressions. Please re-/read section 1.7 - 1.9 in the book for better understanding but I will also clarify here.
An **environment** is a collection of variable and function bindings that map identifiers to values or expressions. The **notation** env = { x ↦ v } is used to represent these mappings, where variables point to values. This helps track variable scopes in functional programming.
However, in the task you were also asked to include function declarations, and as far as I can tell, this was not thoroughly covered in the book-chapters, so I will cover it briefly here.
Here are the declarations:
```fsharp
let a = "a";;
let f a = a * a;;
let g b = (f b) + b;;
```

Here is the environment:
```
env = { 
  a->"a",
  f->"the function f that takes argument a",
  g->\b. "the function g that takes argument b"
}
```

or

```
env = { 
  a->"a",
  f->\a . a * a,
  g->\b . f b + b 
}
```
When a binding associates an identifier with a function value, the function is represented using **`\`**, the ASCII notation for a lambda abstraction. This notation denotes an anonymous function stored as a value in the environment. The identifier following **`\`** represents the function's parameter, while the ****`.`**** separates the parameter from the function body. The function body describes how the result is computed when the function is applied.

or

```
env = {
  a ↦ "a",
  f ↦ λa . (a * a),
  g ↦ λb . (f b) + b
}
```
In the environment notation, **lambda notation** represents function bindings using **anonymous functions (λ-expressions)**.  
A function like:

```fsharp
let f a = a * a;;
```
can be rewritten in **lambda notation** as:

```
f ↦ λa . (a * a)
```

which explicitly shows that `f` is a function taking an argument `a` and returning `a * a`.

```
λx . expression
```
- The function **parameter** is written after `λ` (lambda).
- The **body** of the function follows the `.` (dot).

## Types
There seems to be some confusion as to what types the different expressions have in Ex. 1.8. 
Let's take use fact(fact 4). Remember functions are first-class citizens in F#. The function fact is simply a value like 5, “hello” or false. The type of fact is int -> int, meaning that it expects an integer input and outputs an integer. Once fact is called with an input, the expression fact 4 now has type int. the same goes for the expression fact (fact 4).

This may stem from a confusion of the type notations. You will have seen the symbols -> and * a couple of times now in type expressions such as int * int -> int. I will briefly explain what they mean.
In **F#**,  
- **`->` (arrow notation)** is used for **function types**, meaning a function that takes an input and returns an output.  
  - Example: `int -> string` means a function that takes an `int` and returns a `string`.  
  - Example: `(int -> int) -> int` means a function that takes another function (`int -> int`) as input and returns an `int`.  

- **`*` (tuple notation)** represents **product types**, meaning multiple values grouped together in a tuple.  
  - Example: `int * string` represents a tuple `(5, "hello")`.  
  - Example: `float * float -> float` means a function that takes a tuple `(x, y)` of floats and returns a float.
