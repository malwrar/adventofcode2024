import System.Environment (getArgs)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) =
    qsort [a | a <- xs, a <= x] ++ [x] ++ qsort [a | a <- xs, a > x]

pairDistances :: Num a => [a] -> [a] -> [a]
pairDistances xs ys = zipWith (\x y -> abs (x - y)) xs ys

solution1 :: [Int] -> [Int] -> Int
solution1 xs ys = sum (pairDistances (qsort xs) (qsort ys))

solution2 :: [Int] -> [Int] -> Int
solution2 xs ys = sum [ x * count x ys | x <- xs ]
  where
    count y ys = length [ z | z <- ys, z == y ]

parseInput :: String -> ([Int], [Int])
parseInput contents =
    let ls = lines contents
        pairs = map (map read . words) ls :: [[Int]]
        xs = map head pairs
        ys = map (!! 1) pairs
    in (xs, ys)

main :: IO ()
main = do
    args <- getArgs
    contents <- if null args
        then getContents
        else readFile (head args)
    let (xs, ys) = parseInput contents

    putStrLn ("solution1: " ++ show (solution1 xs ys))
    putStrLn ("solution2: " ++ show (solution2 xs ys))


