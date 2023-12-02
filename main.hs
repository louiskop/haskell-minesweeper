
import System.IO
import System.Random
 
type Coord = (Int, Int)

-- create empty board
createBoard :: Coord -> [[Char]]
createBoard (rows, cols) = replicate rows $ replicate cols '▯'

-- create mines layout given a probability and board of probabilities
createMines :: Coord -> Float -> [[Float]] -> [[Bool]]
createMines (rows, cols) p probabilities = map (\row -> (map (\innerProb -> innerProb <= p) row)) probabilities

-- generate board of random probabilities
genProbabilities :: Coord -> IO [[Float]]
genProbabilities (rows, cols) = do
    gen <- newStdGen
    let probs = take (rows * cols) $ randoms gen :: [Float]
    return $ splitProbs cols probs
    where
        splitProbs _ [] = []
        splitProbs n xs = take n xs : splitProbs n (drop n xs) 


-- convert list to Coordinate
listToCoord :: [Int] -> Coord
listToCoord [x, y] = (x,y)

-- parse user input into a Coordinate
parseCoord :: String -> Coord
parseCoord raw = listToCoord $ map read $ words raw

-- edit the row that was clicked on
clickRow :: [Char] -> Int -> [Char]
clickRow (x: xs) i
                | i == 0 = ('█' : xs)
                | otherwise = (x: clickRow xs (i-1))

-- click on a block 
clickBlock :: [[Char]] -> Coord -> [[Char]]
clickBlock (x: xs) (row, col) 
                | row == 0 = ( (clickRow x col) : xs)
                | otherwise = (x: clickBlock xs (row-1, col)) 

-- this is the main game loop
gameLoop :: [[Char]] -> IO()
gameLoop board = do
    -- Print board
    putStrLn ""
    mapM_ putStrLn board
    putStrLn ""
    
    -- Get input from user
    putStr "Select block: <row> <col>:"
    hFlush stdout
    raw <- getLine
    coord <- return $ parseCoord raw

    -- alter board 
    board <- return $ clickBlock board coord
    gameLoop board


main :: IO()
main = do

    putStrLn "Minesweeper in haskell"  
    putStrLn ""
    
    --  get grid size from user
    putStr "Specify grid size: <row> <col>:" 
    hFlush stdout
    raw <- getLine
    grid <- return $ parseCoord raw
    

    -- create board and mine layout
    board <- return $ createBoard grid
    probabilities <- genProbabilities grid
    mines <- return $ createMines grid 0.4 probabilities

    -- Debugging: print the boolean board for mines
    putStrLn $ unlines $ map (concatMap (\b -> if b then "True " else "False ")) mines

    -- run the game loop 
    gameLoop board

    putStr "Goodbye"   














