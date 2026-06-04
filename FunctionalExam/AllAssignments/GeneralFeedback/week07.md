# General feedback -- Assignment 7

## 7.2
This assignment was mostly well completed, though, with minor issues.
### 7.1 postfix
For that one there isn't much to mention, just make sure to follow the exercise. In this case, the exercise doesn't asks for the printing of extra parenthesis "()".

### 7.2.1 intpInstr
For this part there has been two minor issues that could be improved. Firstly, using nested pattern match for exception handling even though the same exception message is provided for all the branches, e.g.:
```fsharp
match instr with
    | ADD ->
        match stack with
        | a :: b :: rest -> (b + a) :: rest
        | _ -> failwith "Error"
    | SUB ->
        match stack with
        | a :: b :: rest -> (b - a) :: rest
        | _ -> failwith "Error"
```
We will either need to provide different error messages or remove redundant code, for instance:
```fsharp
match (instr, stack) with
| (PUSH x, _) -> ...
| (EXP, _::_) -> ...
| (LOG, _::_) -> ...
| (COS, _::_) -> ...
| (SIN, _::_) -> ...
| (DIV, _::_::_) -> ...
| (MULT, _::_::_) -> ...
| (SUB, _::_::_) -> ...
| (ADD, _::_::_) -> ...
| _ -> failwith "Error"
```
In this case we are matching both the instruction and the stack, i.e.,
the first Div, Mult, Sub, and Add will make sure there are at least two elements
in the stack, the Exp, Log, Cos, and Sin will match only if there is at least one
element in the stack, and the Push will match regardless the number of elements in the stack.
In case those constraints are not met, the last clause/branch will be matched and it will
fail with an error message. 

The second minor issue was regarding the division by zero. This one is not the biggest issue there is, but it's always important to look out for such bugs in your code and provide your own exception handling logic. The issue lies in the following line of code
`| (DIV, a::b::_) -> a/b`
Instead of dividing immediately we can provide an extra guard ensuring that b is not zero.

### 7.2.2 intpProg
This part was mostly done correctly, however, it's sometimes easy to forget that a list (i.e. the stack) can be empty, therefore, it is important to provide a pattern match that accounts for such case.



## 7.3
The common points of confusion this week seemed to center on 7.3. 

This exercise was about creating a library module. 

### 7.3 Overall context: 

First, let us remind ourselves why we would want to create a library module. The point is to write a piece of code that we or others can use without needing to understand works/the details of its implementation. That means abstracting away and hiding details. We define a clear interface for the outside world in the .fsi specification file and hide the internal mess in the .fs implementation file.
These files need to match. Any declaration in the .fsi file must be implemented in the .fs file. But importantly, only things declared in the .fsi file will be available to users of the library, and everything else stays hidden. 

### 7.3 Abstract type: 

So, if we write something like “type ComplexNumber” in the .fsi file without giving an internal structure (like a record type or a discriminated union type) then the actual structure will be hidden from users of the library module. This is what we want: the users of the library do not need to know that a complex number is implemented as a tuple or a record or anything else. They just need to know the type exists, and what functions they can use on it. 

### 7.3 Internal constructor access: 

Some of solutions have hidden the specific type implementation by leaving out the type expression in the .fsi, but have then used the internal constructor of the type in order to create instances of the type anyhow. Since the type is left abstract in the .fsi specification file with no type expression, the internal constructor is also hidden. Instead of trying to create instances of a Complex number by writing something like 
```fsharp 
C(1.0, 2.0))
``` 
we would have to provide a ‘make’ function in both the .fs and .fsi files which takes the necessary arguments and returns a ComplexNumber type. 

```fsharp
val make : float * float -> ComplexNumber   	// in .fsi
let make (a, b) = C(a, b)                       // in .fs 
```

### 7.3 operators: 

Many solutions defined operators like +, -, *, / directly in the module. This will work, but it has some issues: If the user opens the module by writing “open ComplexNumbers” (or whatever the name of the module is), those definitions pollute the namespace. That is, they might shadow built-in operators (like + for integers), or other things with the same name. Big deal - we can just choose not to open the module. This is true, but then we lose the ability to use the infix operators we defined in an infix style. We have to write things like 

```fsharp 
ComplexNumbers.(+) c1 c2
```
Not exactly very elegant.


Section 7.3 in the book lays out some alternative approaches to a solution that avoids these issues, where operators are defined as static members on the type itself, and in this way avoid polluting the name space, and can be used infix style without even needing to open the module. Take another look at section 7.2 and 7.3 if you are confused about this!
