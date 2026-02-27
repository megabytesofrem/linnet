# linnet
Small purely functional language inspired by Rust and Haskell

## Features
- Pure immutability
- Implicitly monadic control flow via blocks (`!{ }`)
- Zero `try/catch`, monadic error types are used instead

## Syntax

```rs
fn safe_div (n: Int) (m: Int) -> Maybe[Int] = 
    if m == 0 || n == 0
      then Nothing
      else Just (m / n)

fn main () -> IO[Unit] = !{
    let shape = Circle(15.0)

    div_result <- safe_div 10 0
    match div_result {
        Just r  -> print $ "Result: " ++ show r
        Nothing -> print "Division by 0!"
    }

    // Rust/Haskell style pattern matching
    let describe = match shape {
        Circle(radius) -> "Radius: " ++ radius 
        Square(x, y)   -> "Size: " ++ x ++ "*" ++ y
    }
}
```

## Type system
Linnet uses a purely monadic effect system, inspired by Haskell.