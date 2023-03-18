add :: Int -> Int -> Int -> Int
add x y z = x + y + z

sub :: Int -> Int -> Int
sub x y = x - y

multiply :: Int -> Int -> Int
multiply x y = x * y

divide :: Int -> Int -> Int
divide x y = x `div` y

operate :: (Int -> Int -> Int) -> Int -> Int -> Int
operate f x y = f x y

main :: IO ()
main = do
    let x = 10
    let y = 5
    let z = 2
    let w = add (add x y z) y z
    let a = w `sub` z
    print a