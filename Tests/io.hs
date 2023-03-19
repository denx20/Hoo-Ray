-- A function with IO

newtype Summary = Summary Int

unwrapSummary :: Summary -> Int
unwrapSummary (Summary n) = n

-- Define functions
clean_files :: IO Summary
clean_files = do
    print "hello" -- This could be any type of IO function
    return (Summary 5)

complex_evaluation :: Summary -> Int
complex_evaluation x = unwrapSummary x

semantic_analysis :: IO Int
semantic_analysis = do
    print "ok"
    return 10

-- Execute them
main :: IO ()
main = do
    x <- clean_files
    let y = complex_evaluation x
    z <- semantic_analysis
    print (y, z)