# linnet
Small purely functional language inspired by ML and Haskell

## Features
- Everything is immutable
- Functions are first-class citizens
- An insanely powerful type system

## Goal
The end goal is to develop an incredibly powerful function programming language
that produces 100% memory-safe C code.

## Syntax

```rs
safeDiv : Int -> Int -> Maybe Int
def safeDiv n m = 
  if n == 0 or m == 0 
     then $ None
     else $ Some (n / m)

main : () -> IO ()
def main = do
  let shape = Circle(15.0)

  div_result <- safe_div 10 0
  match div_result
    | Some r  -> print $ "Result: " ++ show r
    | None    -> print "Division by 0!"

  // Rust/Haskell style pattern matching
  let describe = match shape
    | Circle(radius) -> "Radius: " ++ radius 
    | Square(x, y)   -> "Size: " ++ x ++ "*" ++ y
end
```

## Type system
The type system is based on System F with the F omega extension, which allows for higher kinded types and direct encoding of
category theory concepts (semigroups, monoids, functors, applicative functors etc).