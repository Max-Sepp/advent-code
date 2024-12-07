module Main where
    
import Text.Parsec
import Text.Parsec.String (Parser)

-- Part 1 Solution
numberParser :: Parser Int
numberParser = read <$> (many1 digit)

mulParser :: Parser Int
mulParser = do
    _ <- string "mul("
    num1 <- numberParser
    _ <- char ','
    num2 <- numberParser
    _ <- char ')'
    rest <- solveParser
    return (num1 * num2 + rest)

nextParser :: Parser Int
nextParser = do
    _ <- anyChar
    result <- solveParser
    return result

end :: Parser Int
end = do
    _ <- eof
    return 0

-- solveParser :: Parser Int
-- solveParser = try end <|> try mulParser <|> nextParser

-- Part 2 Solution

consumeUntilDoParser :: Parser String
consumeUntilDoParser = manyTill anyChar (try (string "do()"))

dontParser :: Parser Int
dontParser = do
    _ <- string "don't()"
    _ <- consumeUntilDoParser
    result <- solveParser
    return result

solveParser :: Parser Int
solveParser = try end <|> try dontParser <|> try mulParser <|> nextParser

main :: IO ()
main = do
    inputStr <- readFile "input.txt"
    let result = parse solveParser "" inputStr
    case result of
        Left err -> print err
        Right val -> print val


