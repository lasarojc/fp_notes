

where é usado em  fib2.hs

let?


Fibonacci pra cima

```hs
--8<--
docs/code/fib1.hs
--8<--
```

Collatz

```hs
--8<--
docs/code/collatz3.hs
--8<--
```


- bmi é calculado só uma vez. mais fácil de atualizar os testes. http://learnyouahaskell.com/syntax-in-functions
        bmiTell :: (RealFloat a) => a -> a -> String  
        bmiTell weight height  
            | bmi <= skinny = "You're underweight, you emo, you!"  
            | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
            | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
            | otherwise     = "You're a whale, congratulations!"  
            where bmi = weight / height ^ 2  
                  skinny = 18.5  
                  normal = 25.0  
                  fat = 30.0  