import System.IO

main :: IO ()
main = do 
    inputStr <- readFile "input.txt"
    input = map read (map words (lines inputStr)) :: [[Int]]


solution :: [[Int]] -> Int
solution input = length . (filter (==True)) . (map check)
    where
        check :: [Int] -> Bool
        check xs = (isDecreasing xs || isIncreasing xs) && diffInBounds xs
        isIncreasing xs = all (zipWith (<) xs (tail xs))
        isDecreasing xs = all (zipWith (>) xs (tail xs)) 
        diffInBounds xs = all (zipWith (\x y -> abs(x - y) <= 3) xs tail xs)
        
        