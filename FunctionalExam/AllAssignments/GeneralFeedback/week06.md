# Assignment 6 General Feedback

## General comments

Please remember to annotate code well. When we grade your assignments, well annotated code helps us find relevant information. Also make sure to fix any compiler errors before handing in.

Also please do not rename variables, types, or methods which names are specified or given by the exercises. Renaming adds a layer of friction when grading, since we might have to figure out what method the renamed one corresponds to.

## Exercise 6.3

Most of the confusion for this weeks exercises have been centered around exercise 6.3. The goal of this exercise is to explain what would have to be changed to implement the expression `inc(x)` in `aExp`. The functionality of `inc(x)` is intended to be, that the variable with the name `x` is increased by one and afterwards returned. 

Maps (and basically all other collection types usually used) in f# are immutable. Therefore, the modified maps need to be returned and used by the other methods. For this assignment specifically, this necessitates that the `A` method not only returns an int, but also the modified state. The greater degree of complexity now begins to show itself, as every other instance in the code where `A` is called now need to handle a tuple return value. 

Additionally, the `B` method will also have to return the modified state, as booloean expressions can now potentially contain arithmetic expressions which modify the state.

Also, take a look at the following line of code:
```
    | Eq(a1, a2) -> A a1 s = A a2 s
```
This is an implementation of the `Eq` expression in the `B` method. What happens if `a1` contains an `inc` expression? and what if `a2` references the variable that should be incremented? Also, how do we get the state which should be returned? Please try to consider these questions to better understand what complications implementinc `inc` may cause.

