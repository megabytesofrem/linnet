# linnet
Small purely functional language inspired by ML and Haskell

## Features
- Pure immutability
- Implicitly monadic control flow via blocks (`!{ }`)
- Zero `try/catch`, monadic error types are used instead

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
Linnet uses a purely monadic effect system, inspired by Haskell.