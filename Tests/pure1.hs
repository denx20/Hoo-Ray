{-# LANGUAGE LambdaCase #-}

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

multiply :: Int -> Int -> Int
multiply x z = x * z

sub :: Int -> Int -> Int -> Int
sub k y = k - y

increment :: Int -> Int
increment x = x + 1

decrement :: Int -> Int
decrement x = x - 1

main :: IO ()
main = do
    let x = 10
    let y = 5
    let z = 3
    let w = add x y z
    let k = multiply x z
    let a = k `sub` y

    let b = increment a
    let c = decrement b

    let d = add a b c
    let e = multiply d w
    let f = sub e y
    let g = add f x

    let h = increment g
    let i = decrement h

    let j = add g h i
    let k = multiply j f
    let l = sub k h
    let m = add l g

    let n = increment m
    let o = decrement n

    let p = add m n o
    let q = multiply p l
    let r = sub q n
    let s = add r m

    let t = increment s
    let u = decrement t

    let v = add s t u
    let w' = multiply v r
    let x' = sub w' t
    let y' = add x' s

    print y'
