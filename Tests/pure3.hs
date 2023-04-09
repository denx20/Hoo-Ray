{-# LANGUAGE LambdaCase #-}

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

multiply :: Int -> Int -> Int
multiply x z = x * z

sub :: Int -> Int -> Int -> Int
sub k y = k - y

increment :: Int -> Int
increment x = x + 1

main :: IO ()
main = do
    let x = 10
    let y = 5
    let z = 3
    let w = add x y z
    let k = multiply x z
    let a = k `sub` y

    let b = increment a
    let c = increment b

    let d = add a b c
    let e = multiply d w
    let f = sub e y
    let g = add f x

    let h = increment g
    let i = increment h

    let j = add g h i
    let k' = multiply j f
    let l = sub k' h
    let m = add l g

    print m
