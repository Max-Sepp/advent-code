import System.IO

main :: IO ()
main = do 
    inputStr <- readFile "input.txt"
    let input = map (map read . words) (lines inputStr) :: [[Int]]
    print $ solution input


solution :: [[Int]] -> Int
solution = length . (filter (==True)) . (map check)
    where
        check :: [Int] -> Bool
        check xs = 
            (atMostTwoFalse (zipWith (&&) (isIncreasing xs) (diffInBounds xs))) ||
            (atMostTwoFalse (zipWith (&&) (isDecreasing xs) (diffInBounds xs)))

isIncreasing :: [Int] -> [Bool]
isIncreasing xs = zipWith (<) xs (tail xs)
isDecreasing :: [Int] -> [Bool]
isDecreasing xs = zipWith (>) xs (tail xs)
diffInBounds :: [Int] -> [Bool]
diffInBounds xs = zipWith (\x y -> abs(x - y) <= 3) xs (tail xs)

atMostTwoFalse :: [Bool] -> Bool
atMostTwoFalse = and . (drop 2) . (dropWhile (==True))