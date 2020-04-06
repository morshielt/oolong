# Deklaracja języka imperatywnego *oolong*
__Maria Oparka 394632__

*oolong* to imperatywny język oparty o składnię języka *Latte*.


## Gramatyka
Gramatyka znajduje się załączonym w pliku `oolong.cf`.

# TODO konflikt


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
Standardowe, można dodawać operatorem `+`.

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

