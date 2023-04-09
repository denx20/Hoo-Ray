-- A function with only pure functions (i.e. only `let`s in main)
add :: Int -> Int -> Int -> Int
add x y z = x + y + z

sub :: Int -> Int -> Int
sub x y = x - y

multiply :: Int -> Int -> Int
multiply x y = x * y

divide :: Int -> Int -> Int
divide x y = x `div` y

main :: IO ()
main = do
    let x = 10
    let y = 5
    let z = 3
    let w = add x y z
    let k = multiply x z
    let a = k `sub` y
    print