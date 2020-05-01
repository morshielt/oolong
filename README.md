# Deklaracja języka imperatywnego *oolong*
__Maria Oparka 394632__

*oolong* to imperatywny język oparty o składnię języka *Latte*.


Interpreter opiera się na połączeniu monad `State`, `Reader` oraz `Except`.
`Reader` przechowuje mapę: nazwa zmiennej/funkcji -> lokacja.
`State` przechowuje mapę: lokacja zmiennej -> wartość zmiennej
oraz liczbę reprezentującą kolejną wolną lokację.
`Except` odpowiada za komunikaty błędów.

Moduł statycznego typowania wykorzustuje połączenie monad `Reader` i `Except`.
`Reader` przechowuje:
- mapę: nazwa zmiennej/funkcji -> (typ zmiennej/funkcji, numer reprezentujący, w którym scope została zadeklarowana dana zmienna) 
- aktualny numer scope (stopień zagnieżdżenia)
- oczekiwany typ zwracany przez aktualnie sprawdzaną funkcję oraz nazwa tej funkcji
- informacja czy program w danym momencie wykonuje instrukcję znajdującą się w jakiejś pętli
`Except` odpowiada za komunikaty błędów.


## Gramatyka
Gramatyka to nieco zmodyfikowana gramatyka języka *Latte*.

W gramatyce występuje konflikt:
wyrażenie `if (cond1) f1(); if (cond2) f2(); else g();` jest parsowane tak, że `else` należy do __drugiego__ `if`.

Gramatyka w notacji EBNF:
```
-- programs ------------------------------------------------

entrypoints Program ;

Program.   Program ::= [Stmt] ;

-- statements ----------------------------------------------

Block.     Block ::= "{" [Stmt] "}" ;

separator  Stmt "" ;

Empty.     Stmt ::= ";" ;

BStmt.     Stmt ::= Block ;

Decl.      Stmt ::= Type [Item] ";" ;

DefaultInit.    Item ::= Ident ;

Init.      Item ::= Ident "=" Expr ;

separator nonempty Item "," ;

Ass.       Stmt ::= Ident "=" Expr  ";" ;

Incr.      Stmt ::= Ident "++"  ";" ;

Decr.      Stmt ::= Ident "--"  ";" ;

Ret.       Stmt ::= "return" Expr ";" ;

VRet.      Stmt ::= "return" ";" ;

Cond.      Stmt ::= "if" "(" Expr ")" Stmt  ;

CondElse.  Stmt ::= "if" "(" Expr ")" Stmt "else" Stmt  ;

While.     Stmt ::= "while" "(" Expr ")" Stmt ;

Break.      Stmt ::= "break" ";" ;

Continue.      Stmt ::= "continue" ";" ;

SPrint.      Stmt ::= "print" "(" Expr ")" ";" ;

SExp.      Stmt ::= Expr  ";" ;

FnDef.	   Stmt ::= Type Ident "(" [Arg] ")" Block ;

Arg. 	   Arg ::= Type Ident;

RefArg.	   Arg ::= Type "&" Ident;

separator  Arg "," ;

-- Types ---------------------------------------------------

Int.       Type ::= "int" ;

Str.       Type ::= "string" ;

Bool.      Type ::= "bool" ;

Void.      Type ::= "void" ;

Fun.       Type ::= "<""(" [ByValOrRef] ")" ":" Type ">";

ByVal.  ByValOrRef ::= Type ;

ByRef.  ByValOrRef ::=  Type "&" ;

separator      ByValOrRef "," ;

-- Expressions ---------------------------------------------

ELambda.   Expr6 ::= "(" [Arg] ")" ":" Type "->" Block ;

EVar.      Expr6 ::= Ident ;

ELitInt.   Expr6 ::= Integer ;

ELitTrue.  Expr6 ::= "true" ;

ELitFalse. Expr6 ::= "false" ;

EApp.      Expr6 ::= Ident "(" [Expr] ")" ;

EString.   Expr6 ::= String ;

Neg.       Expr5 ::= "-" Expr6 ;

Not.       Expr5 ::= "!" Expr6 ;

EMul.      Expr4 ::= Expr4 MulOp Expr5 ;

EAdd.      Expr3 ::= Expr3 AddOp Expr4 ;

ERel.      Expr2 ::= Expr2 RelOp Expr3 ;

EAnd.      Expr1 ::= Expr2 "&&" Expr1 ;

EOr.       Expr ::= Expr1 "||" Expr ;

coercions  Expr 6 ;

separator  Expr "," ;

-- operators -----------------------------------------------

Plus.      AddOp ::= "+" ;

Minus.     AddOp ::= "-" ;

Times.     MulOp ::= "*" ;

Div.       MulOp ::= "/" ;

Mod.       MulOp ::= "%" ;

LTH.       RelOp ::= "<" ;

LE.        RelOp ::= "<=" ;

GTH.       RelOp ::= ">" ;

GE.        RelOp ::= ">=" ;

EQU.       RelOp ::= "==" ;

NE.        RelOp ::= "!=" ;

-- comments ------------------------------------------------

comment    "#" ;

comment    "//" ;

comment    "/*" "*/" ;


```


## Tabela cech języka imperatywnego:
Na 15 punktów
- [x]    01 (trzy typy)
- [x]    02 (literały, arytmetyka, porównania)
- [x]    03 (zmienne, przypisanie)
- [x]    04 (print)
- [x]    05 (while, if)
- [x]    06 (funkcje lub procedury, rekurencja)
- [x]    07 (przez zmienną / przez wartość / in/out)
- [ ]    08 (zmienne read-only i pętla for) 
     
Na 20 punktów
- [x]    09 (przesłanianie i statyczne wiązanie)
- [x]    10 (obsługa błędów wykonania)
- [x]    11 (funkcje zwracające wartość)

Na 30 punktów
- [x]    12 (4) (statyczne typowanie)
- [x]    13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
- [ ]    14 (1) (rekordy/tablice/listy)
- [ ]    15 (2) (krotki z przypisaniem)
- [x]    16 (1) (break, continue)
- [x]    17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
- [ ]    18 (3) (generatory)

Razem: 20 + (4 + 2 + 1 + 4) = 31 => __max. 30 punktów__


## Opis języka
### Struktura programu
Program jest listą instrukcji oddzielonych średnikiem.

### Instrukcje
Instrukcje: pusta, złożona, `if`, `while`, `return`, `break`, `continue` jak w C/Javie. Dodatkowo instrukcjami są przypisanie, definicja funkcji (standardowa składnia jak w C), postinkrementacja, postdekrementacja, wypisanie na standardowe wyjście (instrukcja `print`).

### Typy
`int`, `bool`, `void`, `string` jak w Javie. Dodatkowo definiuję typ dla funkcji: `<([typ_argumentu]) : typ_zwracany>`

np. `<(int, string) : bool>` to typ funkcji przyjmującej 2 argumenty (typu `int` oraz `string`) i zwracającej wartość typu `bool`

Domyślne wartości typów przy deklaracji bez inicjalizacji zmiennych:
+ `int` => `0`
+ `bool` => `false`
+ `string` => `""`

Deklaracja bez inicjalizacji zmiennej typu funkcyjnego jest __niedozwolona__.

### Wyrażenia
Takie jak w języku *Latte*, a dodatkowo wyrażenia lambda (funkcje anonimowe) o składni:
`([typ_i_nazwa_argumentu]) : typ_zwracany -> {[instrukcje]}`

np.
```
int x = 5;
< (int) : int > add2  = (int y) : int -> { return y + 2; };

print(add2(x)); // 7
```

### Napisy
Standardowe, można konkatenować operatorem `+`.

### Predefiniowane funkcje
Funkcja `print` wypisująca wyrażenie podstawowego typu (`int`, `bool`, `string`) (__nie `void` ani funkcje anonimowe__)

### Parametry funkcji (przez wartość/referencję)
Parametry funkcji można przekazywać przez wartość lub przez referencję.

Przez wartość:
```c
int x = 5;
int mult3 (int y) {
    y = y * 3;
    return y;
}

print(mult3(x)); // 15
print(x); // 5
```

Przez referencję (w definicji funkcji, między typem a nazwą argumentu należy napisać znak `&`, jak w C++):
```c
int x = 5;
int mult3 (int &y) { // jedyna różnica z przykładem wyżej to '&'
    y = y * 3;
    return y;
}

print(mult3(x)); // 15
print(x); // 15

print(mult3(5)); // ERROR - argument musi być zmienną, nie typem podstawowym
```

### Przesłanianie zmiennych / zagnieżdżanie funkcji
Można przesłaniać zmienne w zagnieżdżonych blokach i zagnieżdżać funkcje.

Przesłanianie:
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
Zagnieżdżanie funkcji:
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

### Zwracanie funkcji / przekazywanie jako parametr / funkcje anonimowe / domknięcia
Zwracanie funkcji / domknięcie:
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

Przekazywanie funkcji jako parametr:
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

### Błędy wykonania
Obsługiwane są błędy wykonania, np. dzielenie przez 0.

### Statyczne typowanie
Interpreter sprawdza zgodność typów przed wykonaniem programu.

