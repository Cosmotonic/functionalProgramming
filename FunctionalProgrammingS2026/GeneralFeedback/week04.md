

# General Feedback for Assignment 4  

## Overall Comments  

Many of you are not commenting on your code nor handling exceptions. It’s important to develop the habit of adding brief comments for each function, even if it’s just a short description of what it does. In top of that, you need to consider cases where the input of the function could lead to exceptions. Every assignment introduction includes the guidelines:  

> "It is important that you annotate your own code with comments…" 

> "You also need to consider scenarios where your solutions should return an error, i.e., an
exception. The requirement is, that no matter what input you pass to your function that fulfils the function type,
then the function should return the intended answer or an exception"

This will also be required in the exam.  

Additionally, we’ve noticed that some solutions rely heavily on `let` expressions in a way that resembles an imperative programming style—similar to writing methods in Java. This course is an opportunity to embrace the declarative nature of functional programming, which allows for more concise and expressive code. Consider the following example, which demonstrates two approaches to the same function:  

```fsharp
// More imperative approach
let toUpper (s: string) : string =
    let exploded = explode s
    let mapped = List.map System.Char.ToUpper exploded
    let result = implode mapped
    result

// More functional approach
let toUpper (s: string) = implode (List.map System.Char.ToUpper (explode s))
```

The second version is more compact and idiomatic in functional programming. Try to take advantage of F#’s functional constructs when writing your solutions.  

---

## 4.2
Some of you have had issues with the fold and foldBack for a quick overview, go to the end of this section. To understand the difference between them, let's investigate both of them starting with the implementation of foldBack:
```fsharp
let rec foldBack f list acc =
    match list with
    | [] -> acc
    | x :: tail -> f x (foldBack f tail acc)
```
Looking at the implementation of the `foldBack` function we see that it expects three parameters:
### The argument `f`
`f: ('T -> 'State -> 'State)`
The first argument `f`, is a function that takes two arguments, a value 'T which is a polymorphic type and an accumulator 'State that is a polymorphic type as well, i.e., it can be string, list, boolean or any other type of your choice.

### The argument `list`
`list: list<'T>`
The second argument `foldBack` expect is a list of type `list<'T>`. Note that the element of the list must be of type `'T`, i.e., the same as the type which the the function `f` expects.

### The argument `acc`
`acc: 'T`
The last argument represents the accumulator containing the initial value that the function will start with.

To show an example of how the method is used, let's build the first argument i.e. the function `f`.
```fsharp
let foldBackFunc value acc =
    printfn "%s" value // For debugging purposes
    value + acc

let foldBackFunc2 value acc =
    printfn "%s" value // For debugging purposes
    value::acc
```

Now, let's call the foldBack using foldBackFunc
```fsharp
foldBack foldBackFunc [ "a"; "b"; "c" ] ""
```
The output of the code is:
```fsharp
c
b
a
val it: string = "abc"
```
As we can see from the print statements, the first value that is handled is c, then b then a... However the output is still "abc" and not "cba". Let's track the execution of the function step by step to understand what is happening under the hood:

1: \
`foldBack foldBackFunc [ "a"; "b"; "c" ] ""`

2: \
The list is matched with the second clause since it's not empty leading to the following: \
`foldBackFunc "a" (foldBack foldBackFunc ["b"; "c" ] "")`

3: \
before foldBackFunc is executed on "a" i.e. the first element of the list, we need to evaluate the second argument which I wrote inside the parenthesis. This leads to the following: \
`foldBackFunc "a" (foldBackFunc "b" (foldBack foldBackFunc ["c"] ""))`

4: \
Now we again end up in the same situation, i.e., we need to evaluate the foldBack before we can evaluate foldBackFunc on "b". This leads to the following: \
`foldBackFunc "a" (foldBackFunc "b" (foldBackFunc "c" (foldBack foldBackFunc [] ""))) `

5: \
The same situation arise again, but calling the foldBack now executes the first clause (i.e., the base case) since the list is empty leading to the following results: \
`foldBackFunc "a" (foldBackFunc "b" (foldBackFunc "c" ""))`  \
Notice that the result of executing `foldBack foldBackFunc [] ""` is just the value of the accumulator i.e. `""`.

6: \
Now the inner most call is executed first since it is necessary for the evaluation of the outer calls: \
`foldBackFunc "a" (foldBackFunc "b" "c")` \
Since we applied the foldBackFunc on "c", thus "c" has been written to the terminal. This explains why the last element is printed first. (Because we start operating only when we reach the last element, and we operate on the last element before we are able to operate on the previous ones).

7: \
`foldBackFunc "a" "bc"` \
Now "b" has been printed.

8: \
`abc` \
Now "a" has been printed and we get our final result "abc"

---

To summarize: \
FoldBack reads the first value in the list, in this case `a`. \
Before it send it to the function `foldBackFunc`, it operates on the second value of the list i.e. `b`.
The recursion continues until we reach the last element in the list, thus, the last element in the list will be the first element that will be send to the function `foldBackFunc`.


Notice that modifying the foldBackFunc from `value + acc` to `acc + value` will results in the inverse of the current output, i.e., `cba` instead of `abc` can you explain the reason behind that? Can you try to do the same using foldBackFunc2?

---

Now lets try to do the same with the fold function:
```fsharp
let rec fold f acc list =
    match list with
    | [] -> acc
    | x :: tail -> fold f (f acc x) tail
```
Notice here that `f acc x` is executed before we call fold again. I.e., `f` operates on the first element of the list immediately before handling or looking at the second element of the list. (Extra: since calling the `fold` function again is the last operation in this recursion level, we call the `fold` function a tail-recursive function. This type of functions is faster and less prone to cause StackOverflowException thus can handle larger lists).

Another important distinction to make here is the arguments of the fold function.
### First argument `f`
`f: ('State -> 'T -> 'State)` 
The function takes the accumulator as the first argument, and then the value of type `'T`. That is the opposite of what the foldBack function expect. Be careful of those small nuances as they might lead to issues down the line.
### Second argument ´acc´
´acc: 'State´
The second argument to the ´fold´ function is the accumulator which again is of polymorphic type.
### Third argument `list`
`list: list<'T>`

Let's again create a function that abide to the type of the argument `f` which the `fold` function expect.
```fsharp
let foldFunc acc value =
    printfn "%s" value
    value + acc
```

Let's now track the execution of the following call: \
`List.fold foldFunc "" [ "a"; "b"; "c" ]` \
Notice that we first provide the accumulator `""` and then the list `[ "a"; "b"; "c" ]`.

1: \
The second clause matches the list, thus we get `x="a"` and `tail=["b"; "c"]`
`List.fold foldFunc (foldFunc "" "a") [ "b"; "c" ]`

2: \
The value `a` is printed to the terminal. Then:
`List.fold foldFunc "a" [ "b"; "c" ]`
Notice that the function `foldFunc` is immediately applied on the first element of the list before proceeding with the recursion. The results we get is `"a"`. The value `"a"` is then passed as the accumulator to the next call of `fold`

3: \
`List.fold foldFunc (foldFunc "a" "b") [ "c" ]`

4: \
The value `"b"` is printed to the terminal. \
`List.fold foldFunc "ba" [ "c" ]` \
Notice that the results of foldFunc is `ba` and not `ab`. That's due to the following code `value + acc`. Since the value was `"b"` and the `acc` was `"a"`, thus `"b" + "a" => "ba"`

5: \
`List.fold foldFunc (foldBack "ba" "c") []`

6: \ 
The value `"c"` is printed to the terminal. \
`List.fold foldFunc "cba" []`

7: base case \
`"cba"`


### Differences between fold and foldBack
1: \
`foldBack` iterates through all elements in the list, and start operating on the last element in the list, then goes backward until it reaches the first element in the list, hence its name, `foldBack`. Whereas, `fold` operates immediately on the first element of the list, i.e., it sends the first element of the list to the function `f` and then it goes to the second element in the list until it exhaust all the elements in the list.

2: \
`fold` reverse the order of the input, i.e., `["a"; "b"; "c"]` becomes ´"cba"´. Whereas, `foldBack` keeps the order of the input i.e. `["a"; "b"; "c"]` becomes ´"abc"´. Notice, this behavior can be changed by modifying the code of the function `f`.

3: \
`fold` is fast (tail-recursive) and can handle large lists. Whereas, `foldBack` can only handle relatively small lists and will cause StackOverflowException on larger lists.

4: \
The arguments have different orders in each function. That's a very important distinction to keep in mind!



## Exercise 4.3  

The goal of this exercise was to write the `toUpper` function using different function composition techniques:  

- `toUpper1` should use the forward composition operator `(>>)`.  
- `toUpper2` should mix the pipe-forward operator `(|>)` and the backward composition operator `(<<)`.  

Make sure to carefully follow all instructions to complete each subtask.  

---

What is the pipe-forward operator `(|>)`? \
The idea of this operator is to send and argument to the following function in the hope of making your code more readable. I.e., instead of writing:
`f x` we write `x |> f`. This gets more useful when we are stacking many functions after each other. E.g.: \n
`f (g (h x))` becomes `x |> h |> g |> f`.

You can read more about the pipes operator in section 2.11 of the book.

What is the forward function composition operator? and why do we use it? \
It has similar purpose to the pipe-forward, however, this one operates on functions. For instance, image that you want to create a process/pipeline where you have some data(input), you clean the data (maybe by filtering bad measurements), then you convert the data to a new form before calculating the mean of those data. The code might looks as follows: \
`calculateMean (convertData (filterBadMeasurements input))` \
This code can be converted to the following: \
`input |> (filterBadMeasurements >> convertData >> calculateMean)`\
The pipeline can be seen inside the parenthesis. The input is first send to the `filterBadMeasurements` function, then `convertData` and lastly to the function `calculateMean`. 


Why do we even need the pipes and function composition operators? \
Short answer, this is just another way of writing the same code, hopefully in a more readable and clear way. 

Consider the following code:
```fsharp
let replicate = String.replicate 2
String.length (replicate (string  "so"))
// Output: 4
```
To read that code your eyes need to navigate to the inner most call, i.e., `string "so"`, afterward, it goes back to replicate when it has deciphered the parenthesis successfully. In the end, it understand that the last function to be called is String.length. If we want to convert that code, probably the easiest thing to do would be the following: 
```fsharp
let replicate = String.replicate 2
String.length << replicate << string <| "so"
// Output: 4
```
That code does the same without the need to to reverse the order in which you wrote your original code. A more readable and natural for english speakers or people whose mother tongue reads from left to right would be the following:
```fsharp
let replicate = String.replicate 2
"so" |> (string >> replicate >> String.length)
// Output: 4

// or
"so" |> string |> replicate |> String.length
```
Some people might find the backward pipe and function composition more natural, others would find the pipe-forward combined with the function composition the one that is the most natural. And some would prefer to stick to one type of operators, i.e., the pipe-forward instead of combining two different types. Please use whichever you find most natural for you. 