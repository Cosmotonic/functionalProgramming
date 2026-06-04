### General feedback - week 3. 

### Argument evaluation

In exercise 3.2 there has been some confusion, regarding whether or not `fact(1)` in `test(false, fact(-1))` gets evaluated or not. The key rule to remember here is that in F# **arguments are evaluated before the method is called**.

### Method declaration of curry/uncurry

Some groups have solved the curry method in a way similar to this:

    let curry f = fun x -> fun y -> f(x,y)

While this does work, lets take a step back and look at what the method does. In F# a function with multiple parameters is actually a series of functions, each taking one argument until all arguments are provided. What this means is, that if you call a function with two arguments, but only with one argument, the result is the first method with the argument already given. Take the following method as an example:

    let multiply a b = a * b

This method is of type int -> int -> int and simply multiplies two numbers. If we call this method with one parameter, eg "4", we get a new method which takes one argument and multiplies it by 4:

    let multiplyBy4 = multiply 4

The type of "multiplyBy4" is int -> int, and it simply multiplies the argument given with 4.

Now lets look at the method `let curry f = fun x -> fun y -> f(x,y)` again. The way this method is written, it explicitly specifies that curry f returns a method with one parameter "x" which returns a method taking one parameter "y". Since, as explained earlier, normal methods with multiple parameters does something very similar by default, the curry method can be written in an equivalent more simple way, like this:

    let curry f x y = f (x, y)

Oppositely, any method with multiple parameters can be written as a chain of anonymous methods, eg. "multiply" can be written as `let multiply = fun a -> fun b -> a * b`


### Modularity and keeping functions light. 

One note on the solution for this week's exercise, particularly in 3.6 is that the functions you declare are quite “heavy”. There are many let expressions within the same single function and some repeated code across functions. A better and more modular, functional approach would be to break the logic into smaller reusable functions which you can combine to build the solution logic. This type of function composition, where you use smaller, simpler functions to create more complex functions is often what we aim to do in functional programming. One approach 3.6 in particular might be to create a ‘toPence’ function and a ‘normalize’ function, which you can then reuse for all the arithmetic functions. Equally, in 3.7, you use the first arithmetic functions you declare, addition and multiplication, to build more complex arithmetic operators. 

### Type annotations

Many of the solutions have solved 3.7 using tuples of integers to represent complex numbers, despite the fact that it is stated that complex numbers are represented by a pair of real numbers. The F# type inference will default to assuming the type integer for the built-in operators ‘+’ and ‘-’ etc. This means that, in this case, explicit type annotations are needed to help the type inference. 

### Infix operators

In 3.6 and 3.7 most solutions have correctly implemented infix operators. However, some solutions that have also included test examples use these operators as named functions in a prefix-like manner (for instance “(.+.) amount1 amount2)”. The clever thing about infix operators is that they can be used between values, like “amount1 .+. amount2”, making the syntax cleaner and more intuitive.

### Some groups seem confused by what is asked in Exercise 3.8

You are asked to change this function to have only two clauses: 
Currently, the function has three clauses, corresponding to the three pattern matching cases.

```fsharp
let rec altsum =
    function
    | [] -> 0
    | [ x ] -> x
    | x0 :: xs -> x0 - altsum xs
```

Consider the structure of a list: A list is either empty, and is made of the empty list, ([]) or it is made of a head and a tail (x::xs), where the tail itself is a list, which is either the empty list or made of a head and tail and so on. 
This means that the empty list will not match the pattern matching case x::xs, but the list with one element (which has the structure x::xs, where x = element and tail = []) will match this case. It will do a recursive call with the tail which is the empty list, will match the first pattern-matching case. Therefore, it is not necessary to explicitly look for the list with one element, even though we are destructuring the list into two elements in the third pattern matching case, and we can get rid of the second pattern matching case without altering the behaviour of the function.
