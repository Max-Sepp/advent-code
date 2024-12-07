main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    let input  = map (\(t, xs) -> (read t :: Int, intsToList(tail xs))) (map (span (/=':')) (lines inputStr))
    let output = sum (map (uncurry solve) input)
    print output

intsToList :: String -> [Int]
intsToList = map read . words

solve :: Int -> [Int] -> Int
solve target (x : xs)
    | evaluate xs x = target
    | otherwise     = 0
    where
        evaluate :: [Int] -> Int -> Bool
        evaluate [] acc       = acc == target
        evaluate (x : xs) acc = evaluate xs (x + acc) || evaluate xs (x * acc)