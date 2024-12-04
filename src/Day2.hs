import System.Environment (getArgs)

adjacentPairs xs = zip xs (tail xs)

validDifferences :: [Int] -> Bool
validDifferences xs = all (\(x, y) -> abs (x - y) >= 1 && abs (x - y) <= 3) (adjacentPairs xs)

isMonotonic :: [Int] -> Bool
isMonotonic xs = all (uncurry (<=)) (adjacentPairs xs) || all (uncurry (>=)) (adjacentPairs xs)

isSafe :: [Int] -> Bool
isSafe xs = validDifferences xs && isMonotonic xs

canBeMadeSafe :: [Int] -> Bool
canBeMadeSafe xs = any isSafe [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

solution1 :: [[Int]] -> Int
solution1 reports = length [r | r <- reports, isSafe r]

solution2 :: [[Int]] -> Int
solution2 reports = length [r | r <- reports, isSafe r || canBeMadeSafe r]

parseInput :: String -> [[Int]]
parseInput contents = map (map read . words) (lines contents)

main :: IO ()
main = do
    args <- getArgs
    contents <- if null args
        then getContents
        else readFile (head args)
    let reports = parseInput contents

    putStrLn ("solution1: " ++ show (solution1 reports))
    putStrLn ("solution2: " ++ show (solution2 reports))
