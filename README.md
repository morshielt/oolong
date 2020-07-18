# *oolong* imperative language interpreter
Interpreter is based on a `State`, `Reader` and `Except` monads.
- `Reader` stores map: variable/function name ->  location.
- `State` stores  map: location -> value and a number representing the next free location.
- `Except` is responsible for error messages.

Static typing module uses `Reader` and `Except` monads.
- `Reader` stores:
     - map: variable/function name -> (type, number representing scope of the declaration) 
     - current scope index
     - expected type returned by currently checked function and its name
     - information whether current instruction is inside a loop
- `Except` is responsible for error messages.

## Language description
### Program structure
Program is a list of instructions separated by semicolons.

### Statements
Statements: empty, composed, `if`, `while`, `return`, `break`, `continue` (C/Java-like), declaration, assignment, function definition, post-increment, post-decrement, wrirting to stdout (`print`).

### Types
`int`, `bool`, `void`, `string`, function type: `<([arg_type]) : return_type>`

e.g. `<(int, string) : bool>` is the type of function with 2 arguments (of types `int` and `string`) and returning value of type `bool`

Default types' values when declaring without initializing:
+ `int` => `0`
+ `bool` => `false`
+ `string` => `""`

Declaration without initialization of function type variable is __forbidden__.

### Expressions
Mostly C/Java-like, additionally lamda expressions (anonymous functions) with semantics:
`([arg_type_and_name]) : return_type -> {[statements]}`

e.g.
```
int x = 5;
< (int) : int > add2  = (int y) : int -> { return y + 2; };

print(add2(x)); // 7
```

### Strings
Standard, can be concatenated by `+`.

### Predefined functions
`print` function writing expression of a basic type (`int`, `bool`, `string`) to stdout (__not `void` or anonymous functions__).

### Function parameters
Function parameters can be passed by value or by reference.

By value:
```c
int x = 5;
int mult3 (int y) {
    y = y * 3;
    return y;
}

print(mult3(x)); // 15
print(x); // 5
```

By reference (in function definition, between the type and the name of the argument there should be an `&`, like in C++):
```c
int x = 5;
int mult3 (int &y) { // the only difference with example above is '&'
    y = y * 3;
    return y;
}

print(mult3(x)); // 15
print(x); // 15

print(mult3(5)); // ERROR - argument has to be a variable, not a basic type
```

### Variable shadowing
```c
int x = 5;

int f(int y) {
    return x * y;
}

int g(int z) {
    int x = 1;
    return f(x+z);
}

print(g(1)); // g(1) = f(1+1) = 5 * 2 = 10
```

### Function nesting
```c
int x = 5;
int f(int y) {
    int add2(int z) {
        return z + 2;
    }
    return x * add2(y);
}

print(f(1)); // f(1) = 5 * (1 + 2) = 15; 
```

### Closures
```c
<(int) : int> multByPlus1Function(int multiplier) {
    int m = multiplier + 1;
    return (int x) : int -> {
        return x * m;
    };
}

<(int) : int> multBy5 = multByPlus1Function(4);
print(multBy5(7)); // 35
```

### Functions as parameters
```c
bool big(int x) {
    if (x > 50) return true;
    else return false;
}

bool checkCondition(<(int):bool> cond, int x) {
    return cond(x);
}

print(checkCondition(big, 100)); // true
print(checkCondition((int x) : bool -> { return x == 5; }, 10 )); // false
```

### Runtime exceptions
- Division by 0

### Static typing
Interpreter perfoms a static type check before program execution.

