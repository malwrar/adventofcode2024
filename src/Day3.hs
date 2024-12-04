import System.Environment (getArgs)
import Text.Regex.Posix ((=~))

data Instruction
    = Mul Int Int
    | Enable
    | Disable
    deriving (Show)

data VMState = VMState
    { currentSum   :: Int
    , enabledState :: Bool
    } deriving (Show)

executeInstruction :: Instruction -> VMState -> VMState
executeInstruction (Mul x y) state@(VMState sum enabled)
    | enabled   = state { currentSum = sum + (x * y) }
    | otherwise = state
executeInstruction Enable state = state { enabledState = True }
executeInstruction Disable state = state { enabledState = False }

runVM :: [Instruction] -> VMState -> Bool -> VMState
runVM instructions initialState useEnableDisable =
    foldl (applyInstruction useEnableDisable) initialState instructions
  where
    applyInstruction :: Bool -> VMState -> Instruction -> VMState
    applyInstruction False state (Mul x y) = executeInstruction (Mul x y) state
    applyInstruction False state _ = state -- Ignore Enable/Disable if not used
    applyInstruction True state instruction = executeInstruction instruction state

solution1 :: [Instruction] -> Int
solution1 instructions =
    currentSum $ runVM instructions (VMState 0 True) False

solution2 :: [Instruction] -> Int
solution2 instructions =
    currentSum $ runVM instructions (VMState 0 True) True

parseInput :: String -> [Instruction]
parseInput contents =
    let regex = "(mul\\(([0-9]{1,3}),([0-9]{1,3})\\))|\\b(do)\\b|\\b(don't)\\b"
    in map parseMatch (contents =~ regex :: [[String]])
  where
    parseMatch :: [String] -> Instruction
    parseMatch [_, mulMatch, x, y, doMatch, dontMatch]
        | not (null mulMatch) = Mul (read x) (read y)
        | not (null doMatch)  = Enable
        | not (null dontMatch) = Disable
        | otherwise = error "Invalid instruction format"
    parseMatch _ = error "Unexpected match structure"

main :: IO ()
main = do
    args <- getArgs
    contents <- if null args
        then getContents
        else readFile (head args)
    let instructions = parseInput contents

    putStrLn ("solution1: " ++ show (solution1 instructions))
    putStrLn ("solution2: " ++ show (solution2 instructions))
