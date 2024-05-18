## Best Practices for Functional Programming

### Key Principles:
1. **Pure Functions**: Functions that always produce the same output for the same input and have no side effects.
2. **Immutability**: Data should not be modified after it is created. Use functions that return new data structures instead of modifying existing ones.
3. **Referential Transparency**: Expressions can be replaced with their values without changing the program's behavior.
4. **Higher-Order Functions**: Functions that take other functions as arguments or return them as results.
5. **Function Composition**: Combining simple functions to build more complex ones.
6. **Currying**: Breaking down a function that takes multiple arguments into a series of functions that take single arguments.
7. **Avoid Shared State**: Minimize the use of shared state to reduce the risk of unintended side effects.
8. **Declarative Programming**: Focus on what to do, rather than how to do it.

### TypeScript Examples:

```typescript
// Pure Function
const add = (a: number, b: number): number => a + b;

// Immutability
const originalArray = [1, 2, 3];
const newArray = [...originalArray, 4]; // newArray is [1, 2, 3, 4]

// Higher-Order Function
const map = (fn: (x: number) => number, arr: number[]): number[] => arr.map(fn);

// Function Composition
const compose = (f: Function, g: Function) => (x: any) => f(g(x));
const addOne = (x: number) => x + 1;
const double = (x: number) => x * 2;
const addOneThenDouble = compose(double, addOne);

// Currying
const multiply = (a: number) => (b: number) => a * b;
const double = multiply(2);
```

### Haskell Examples:

```haskell
-- Pure Function
add :: Int -> Int -> Int
add a b = a + b

-- Immutability
originalList = [1, 2, 3]
newList = originalList ++ [4] -- newList is [1, 2, 3, 4]

-- Higher-Order Function
map :: (a -> b) -> [a] -> [b]
map = Prelude.map

-- Function Composition
compose :: (b -> c) -> (a -> b) -> a -> c
compose = (.)
addOne = (+ 1)
double = (* 2)
addOneThenDouble = compose double addOne

-- Currying
multiply :: Int -> Int -> Int
multiply a b = a * b
double = multiply 2
```

### Sources:
- [Principles of Functional Programming - freeCodeCamp](https://www.freecodecamp.org/news/the-principles-of-functional-programming/)
- [What is functional programming? A practical guide | InfoWorld](https://www.infoworld.com/article/3613715/what-is-functional-programming-a-practical-guide.html)
