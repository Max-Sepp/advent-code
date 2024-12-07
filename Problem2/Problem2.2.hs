import System.IO

main :: IO ()
main = do 
    inputStr <- readFile "input.txt"
    let input = map (map read . words) (lines inputStr) :: [[Int]]
    print $ solution input


solution :: [[Int]] -> Int
solution = length . (filter (==True)) . (map check1)

check1 :: [Int] -> Bool
check1 xs = any check (subLists xs)
    where
        check :: [Int] -> Bool
        check xs = (isDecreasing xs || isIncreasing xs) && diffInBounds xs
        isIncreasing :: [Int] -> Bool
        isIncreasing xs = and (zipWith (<) xs (tail xs))
        isDecreasing :: [Int] -> Bool
        isDecreasing xs = and (zipWith (>) xs (tail xs)) 
        diffInBounds :: [Int] -> Bool
        diffInBounds xs = and (zipWith (\x y -> abs(x - y) <= 3) xs (tail xs))
        subLists :: [Int] -> [[Int]]
        subLists xs = go 0 []
            where
                xsLength = length xs
                go :: Int -> [[Int]] -> [[Int]]
                go index acc 
                    | index < xsLength = 
                        go 
                            (index + 1) 
                            ([x | (i, x) <- zip [0..] xs, i /= index ] : acc)
                    | otherwise = acc

-- trying to do optimised version whish does just try removing every element and
-- testing

check2 :: [Int] -> Bool
check2 xs = checkSequence True xs || checkSequence False xs

checkSequence :: Bool -> [Int] -> Bool
checkSequence increasing xs
    | increasing = go (<) xs (needToSkipFirst (<) xs)
    | otherwise  = go (>) xs (needToSkipFirst (>) xs)
    -- where
-- need to write a function that handles the edge case of the first one
-- violating the sequence
needToSkipFirst :: (Int -> Int -> Bool) -> [Int] -> Bool
needToSkipFirst _ []  = False
needToSkipFirst _ [x] = False
needToSkipFirst f (x : y : _) = not (x `f` y && checkDiff x y)

go :: (Int -> Int -> Bool) -> [Int] -> Bool -> Bool
go _ [] _   = True
go _ [x] _  = True
go f [x, y] skipped = not skipped || (x `f` y && checkDiff x y)
go f (x : y : z : xs) skipped
    | x `f` y && checkDiff x y = go f (y : z : xs) skipped
    | not skipped              = go f (x : z : xs) True
    | otherwise                = False



checkDiff :: Int -> Int -> Bool
checkDiff x y = abs (x - y) <= 3

atMostTwoFalse :: [Bool] -> Bool
atMostTwoFalse = and . (drop 2) . (dropWhile (==True))