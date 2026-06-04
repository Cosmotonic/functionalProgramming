# General feedback, assignment 5

Well done on this week's assignment!

One general advice is to test your functions on a few example inputs before handing in to ensure that they work as intended. You're often given at least one example input and its result in the exercise, which you can use to compare to the result of calling your own function with the same input. Most of you already do this - great!

## Pre-order, in-order or post-order?

In 5.2, you were asked to do in-order traversal, but a lot of you did pre-order traversal instead, i.e. you evaluated the node itself before its left and right subtrees:

```F#
let rec mapPreOrder f tree =
    match tree with
    | Leaf -> Leaf
    | Node (n, left, right) ->
        Node (f n, mapInOrder f left, mapInOrder f right)
```

When you traverse a binary tree, you can either do it **pre-order** (work on the node itself first, then the left subtree, then the right subtree), **in-order** (work on the left subtree first, then the node itself, then the right subtree) or **post-order** (work on the left subtree first, then the right subtree, then the node itself). This is about evaluation order: When do we evaluate the node in relation to its left and right subtrees? We can either do it before (pre-order), in-between (in-order), or after (post-order).

```F#
// Pre-order: node, left, right
let rec preOrder tree =
    match tree with
    | Leaf -> []
    | Node (n, left, right) ->
        n :: preOrder left @ preOrder right

// In-order: left, node, right
let rec inOrder tree =
    match tree with
    | Leaf -> []
    | Node (n, left, right) ->
        inOrder left @ n :: inOrder right

// Post-order: left, right, node
let rec postOrder tree =
    match tree with
    | Leaf -> []
    | Node (n, left, right) ->
        postOrder left @ postOrder right @ [n]
```

To solve 5.2 correctly, think of how you could force your function to evaluate the left subtree before the note itself (think `let` statements).

Some of you also forgot to answer when `mapInOrder` might produce a result different from `mapPostOrder`, even though the resulting tree is still structurally the same. You can convince yourself that the order actually does matter if you start thinking about side effects.

### Cons and list concatenation

What's the difference between `::` and `@`? The cons operator `::` is for prepending elements of type `'a` to a list that contains elements of type `'a` - so its first operand is an element of some type, and its second operand is a list that contains elements of the same type. `@` is for concatenating two lists, so both operands are lists.

In other words: `lst @ [x]` means add element `x` at the **back** of the list `lst`. Contrarily to `x::lst`, which means add element `x` at the **front** of the list `lst`. So `[x] @ lst` does the same as `x :: lst`.

## Folding

In 5.3, we saw some groups using `inOrder` to first create an in-order list representation of the input bintree and then applying `List.fold` to it. This works, but you should be aware that this approach processes the tree twice; once when creating the list, and once when mapping/folding over it.

By the way, remember fold and foldBack? If not, here's a walkthrough:

`List.fold` processes a list from left to right. It takes a function (`'State -> 'T -> 'State`), an accumulator (`'State`) and a list (`'T list`), and applies the function to all elements in the list. The accumulator carries intermediate results, so we start usually start with a base value (e.g. `0` for sum, `[]` for list).

`List.fold f acc [x_1; x_2; …; x_n]` returns `f (… (f (f acc x_1) x_2) … x_n-1) x_n`, where the innermost expression (`f acc x_1`) is the accumulator to the next expression (`f (f acc x_1) x_2`) and so on.

```
TRACE OF `fold (-) 0 [1;2;3]`
Function: (-) (the prefix subtraction function)
Acc: 0
List: [1;2;3]

    fold (-) 0 [1;2;3]
        fold (-) ((-) 0 1) [2;3]
        fold (-) -1 [2;3]
            fold (-) ((-) -1 2) [3]
            fold (-) -3 [3]
                fold (-) ((-) -3 3) []
                fold (-) -6 []
    -6
```

`List.foldBack`, on the other hand, processes a list from *right* to *left*. So `List.foldBack f [x_1; x_2; …; x_n] acc` returns `f x1 (f x2 (… f xn-1 (f xn acc)…))`

```
TRACE OF `foldBack (-) [1;2;3] 0`
Function: (-) (the prefix subtraction function)
List: [1;2;3]
Acc: 0

    foldBack (-) [1;2;3] 0
        (-) 1 (foldBack (-) [2;3] 0)
            (-) 1 ((-) 2 (foldBack (-) [3] 0))
                (-) 1 ((-) 2 ((-) 3 (foldBack (-) [] 0)))
                (-) 1 ((-) 2 ((-) 3 (0)))
            (-) 1 ((-) 2 (3 - 0))
            (-) 1 ((-) 2 (3))
        (-) 1 (2 - 3)
        (-) 1 (-1)
    1 - (-1)
    2
```

Note that sum or other *commutative* operations give the same result with `fold` or `foldBack`, but operations like subtraction (above) or building a list differ. For example, reversing a list:

```F#
let reverseList = List.fold (fun acc x -> x :: acc) [] [1;2;3] // [3;2;1]
let preserveList = List.foldBack (fun x acc -> x :: acc) [1;2;3] [] // [1;2;3]
```

### More on List.map, List.fold and List.filter

**This section is copied from last year's general feedback in the bachelor's FP course.**

A higher-order function is a fancy word for functions which takes functions as one or more parameters.

One of the hardest and most important parts of this course is understanding this concept.
However, this is not unique to F#. In Java, some methods also take functions as paramters. For example:

```java
// Initialising an ArrayList of names
ArrayList<String> names = new ArrayList<>(Arrays.asList("Grace Hopper", "Ada Lovelace", "Jean E. Sammet", "Elizabeth J. Feinler"));
// Removing all strings from names, which starts with "J"
names.removeIf(n -> n.startsWith("J"));
System.out.println(names); // ["Grace Hopper", "Ada Lovelace", "Elizabeth J. Feinler"]
```

As you can see, `ArrayList.removeIf` takes a function as a parameter, which returns a boolean value for each element in the ArrayList (this type of lambda function is often called a *predicate*).

If we wanted to define a similiar function in F#, we can simply do something like the following:

```fsharp
let rec removeIf f lst = 
    match lst with
    | x :: xs -> 
        if f (x) then x :: (removeIf f xs) else (removeIf f xs)
    | _ -> []
```

The type parameters could also be explicitly declared: 
```fsharp
let rec removeIf (f : 'a -> bool) (lst : list<'a>) : list<'a> = 
    ...
```
or
```fsharp
let rec removeIf (f : _ -> bool) (lst : list<_>) : list<_> = 
    ...
```

#### List.map

[List.map](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-listmodule.html#map)
is a standard higher-order function in F#. It takes a `mapping`-function of type `'T -> 'U` and a `list` of type `list<'T>`. It returns a something of type `list<'U>`.
/ 
(`'T -> 'U` means a function that takes a parameter of type `'T` and returns something of type `'U`)

It goes through all elements of the given list, applies the function to each element to create a new list (just like the [`Enumerable.Select` in C#](https://learn.microsoft.com/en-us/dotnet/api/system.linq.enumerable.select?view=net-9.0)).

For example, if we wanted to make change all `'e'`-characters in a list of characters to `'a'`-characters, we could do the following:

```fsharp
let asToEs charList = List.map (fun c -> if c = 'e' then 'a' else c) charList
printfn "%A" (asToEs ['J';'a';'p';'a';'n';'e';'s';'e';]) // ['J'; 'a'; 'p'; 'a'; 'n'; 'a'; 's'; 'a';]
```

#### List.fold

[List.fold](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-listmodule.html#fold)
is also a standard higher-order function in F#. It takes a `folder`-function of type `'S -> 'T -> 'S`, a `startingValue` of type `'S` and a `list` of type `list<'T>`. It returns a something of type `list<'S>`.
(Note: In some places the `startingValue` of type `'S` is also described as a `state` of type `'State`. Please don't confuse this with `state` from week 4. `'State` with the `'` in the beginning means that it is not a predefined type, so it can be anything from `int` to `List` to something insane like `List<List<string>*List<int*List<float>>>`)

`List.fold` is weird function, but it is also central to this course, and will be used throughout its entirety.

It's just another way of making foreach loops (a loop which goes through each element of a list and does *something*).

Let's say that we wanted to go through an entire list of characters and count the amount of `e`-characters (maybe we are programming a [game of hangman](https://en.wikipedia.org/wiki/Hangman_(game)))

```fsharp
let countEs (word : list<char>) : int = List.fold (fun acc c -> if c = 'e' then acc + 1 else acc) 0 word
```

Notice that the anonymous function used as the `folder`-function takes **2** arguments. This is because a `folder`-function has something called an accumulator (often shortened to `acc`). This is because `List.fold` builds a new value throughout its execution. Or in normal English: An accumulator is a fancy word for something used to store the result of all previous steps.

Imagine that we, in real life, are to do a big calculation. If I ask you to calculate $349 + 712 + 199 + 573$, you would probably take it step by step and store the previous results:
$$
\begin{align*}
    349 + 712 + 199 + 573 & \\
    = 1061 + 199 + 573 & \\
    = 1260 + 573 & \\
    = 1833 &
\end{align*}
$$

Here, the accumulator is, at first, $1061$ (the result of $349 + 712$). Then it becomes $1260$ (the result of $1061 + 199$). And finally it is $1833$ ($1260 + 573$). The final value of the accumulator is the results! Wow!

This is the same in programming!

```fsharp
let result = List.fold (fun acc n -> acc + n) 0 [349; 712; 199; 573;]
```

##### List.fold used to create a list

Remember our F# implementation of `removeIf` from earlier?

```fsharp
let rec removeIf f lst = 
    match lst with
    | x :: xs -> 
        if f (x) then x :: (removeIf f xs) else (removeIf f xs)
    | _ -> []
```

Notice that we also go through all the elements of `lst` here to create a new list. This indicates we could also do it with `List.fold`!

```fsharp
let rec removeIf f lst = 
    List.fold (fun acc x -> if f (x) then acc @ [x] else acc) List.empty lst
```

(Note: `acc @ [x]` means add element `x` at the **back** of the list `acc`. Contrarily to `x::acc`, which means add element `x` at the **front** of the list `acc`).

#### List.filter

A better way of implementing something like `removeIf` would probably be to use `List.filter`.

[List.filter](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-listmodule.html#filter)
is *also* a standard higher-order function in F#. It takes a `predicate`-function of type `'T -> bool` and a `list` of type `list<'T>`. It returns a new collection containing only the elements of the collection for which the given predicate returns `true`.

```fsharp
let rec removeIf f lst = List.filter f lst
```

Another example would be if we wanted to create a new list without `e`-characters from a list of characters (remember the [game of hangman](https://en.wikipedia.org/wiki/Hangman_(game)) from earlier)

```fsharp
let removeEs (word : list<char>) : list<char> = List.filter (fun c -> c = 'e') word
```

(This is identical to the following, if you have seen something similiar before:)

```fsharp
let removeEs (word : list<char>) : list<char> = List.filter ((=) 'e') word
```
