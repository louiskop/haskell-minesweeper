
import System.IO
import System.Random
import Data.Char
import System.Exit (exitFailure)

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

-- integer to char conversion
intToChar :: Int -> Char
intToChar n = toEnum (n + fromEnum '0')

-- convert list to Coordinate
listToCoord :: [Int] -> Coord
listToCoord [x, y] = (x,y)

-- parse user input into a Coordinate
parseCoord :: String -> Coord
parseCoord raw = listToCoord $ map read $ words raw

-- check if coords are on board
coordInRange :: Coord -> Coord -> Bool
coordInRange (rows, cols) (x, y) = x >= 0 && x < rows && y >= 0 && y < cols

coordNotExploredColumn :: Int -> [Char] -> Bool
coordNotExploredColumn col (x:xs)
                | (col == 0) = (x == '▯')
                | otherwise = coordNotExploredColumn (col - 1) xs

-- check if coord is explored
coordNotExplored :: [[Char]] -> Coord -> Bool
coordNotExplored (x:xs) (row, col)
                | (row == 0) = coordNotExploredColumn col x
                | otherwise = coordNotExplored xs (row-1, col) 

-- check if mine is present at coordinate
checkMineColumn :: [Bool] -> Int -> Bool
checkMineColumn (x: xs) i
                | i == 0 = x
                | otherwise = checkMineColumn xs (i-1)

-- check if mine is present at coordinate
checkMine :: [[Bool]] -> Coord -> Bool
checkMine (x: xs) (row, col)
                | row == 0 = checkMineColumn x col
                | otherwise = checkMine xs (row-1, col)

-- convert booleans to countable ints
countBool :: Bool -> Int
countBool x | x = 1
            | otherwise = 0

-- compute number of mines for adj blocks
countMines :: Coord -> [[Bool]] -> Int
countMines (x, y) (m:ms) = sum $ map (\coord -> countBool $ checkMine (m:ms) coord) $ filter (coordInRange (rows, cols)) coordList
                    where
                        coordList = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]
                        cols = length m
                        rows = length (m:ms)


-- update a block number
updateSingleColumn :: Int -> Coord -> [Char] -> [[Bool]] -> [Char]
updateSingleColumn i (row, col) (x: xs) mines
                    | i == 0 = ( value : xs)
                    | otherwise = (x: (updateSingleColumn (i-1) (row, col) xs mines))
                    where 
                        value = if countMines (row, col) mines > 0 then
                            (intToChar $ countMines (row, col) mines )
                            else '█'

-- update a block number
updateSingle :: Int -> Coord -> [[Char]] -> [[Bool]] -> [[Char]]
updateSingle i (row, col) (x:xs) mines
                    | i == 0 = ( (updateSingleColumn col (row, col) x mines) : xs )
                    | otherwise = (x: (updateSingle (i-1) (row, col) xs mines))

-- explore board and update numbering
updateBoard :: Coord -> [[Bool]] -> [[Char]] -> [[Char]]
updateBoard (row, col) mines (b:bs)
            | (countMines (row, col) mines) <= 0 = foldr (\(x,y) acc -> (updateBoard (x, y) mines acc)) (updateSingle row (row,col) (b:bs) mines) $ filter (coordNotExplored (b:bs)) $ filter (coordInRange (rows, cols)) coordList
            | otherwise = updateSingle row (row, col) (b:bs) mines
            where
                coordList = [(row-1, col-1), (row-1, col), (row-1, col+1), (row, col-1), (row, col+1), (row+1, col-1), (row+1, col), (row+1, col+1)]
                cols = length b
                rows = length (b:bs)

-- check winning board state
winState :: [[Char]] -> [[Bool]] -> Coord -> Bool
winState board mines (rows, cols) = and [ (checkMine mines (x,y)) || (not (coordNotExplored board (x,y))) | x <- [0..(rows-1)] , y <- [0..(cols-1)]]

-- this is the main game loop
gameLoop :: [[Char]] -> [[Bool]] -> IO()
gameLoop board mines = do
    -- print board
    putStrLn ""
    mapM_ putStrLn board
    putStrLn ""

    -- get input from user
    putStr "Select block: <row> <col>:"
    hFlush stdout
    raw <- getLine
    
    if raw == "memo" then do
        -- print the boolean board for mines
        putStrLn $ unlines $ map (concatMap (\b -> if b then "True " else "False ")) mines
        gameLoop board mines
    else do
        putStr ""
    

    -- parse coordinates into tuple
    coord <- return $ parseCoord raw

    -- check if user clicked on a mine
    if checkMine mines coord 
        then do 
            putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
            putStrLn "Oops, you clicked on a mine, game over !"
            putStrLn "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
            exitFailure
        else do 
            -- alter board 
            board <- return $ updateBoard coord mines board
            
            -- check winning state 
            if winState board mines (length board, length (head board)) 
                then do
                    -- print board
                    putStrLn ""
                    mapM_ putStrLn board
                    putStrLn ""

                    -- print winning message and exit
                    putStrLn "++++++++++++++++++++++++++++++++"
                    putStrLn("CONGRATULATIONS, YOU WIN !!!")
                    putStrLn "++++++++++++++++++++++++++++++++"
                    exitFailure
                else do
                    -- run game loop again
                    gameLoop board mines


   
main :: IO()
main = do


    putStrLn "==================================="
    putStrLn "Minesweeper in haskell"  
    putStrLn "==================================="
    putStrLn "Type 'memo' for bomb locations"
    putStrLn ""

    --  get grid size from user
    putStr "Specify grid size: <row> <col>:" 
    hFlush stdout
    raw <- getLine
    grid <- return $ parseCoord raw
    

    -- create board and mine layout
    board <- return $ createBoard grid
    probabilities <- genProbabilities grid
    mines <- return $ createMines grid 0.1 probabilities

    -- run the game loop 
    gameLoop board mines

    putStr "Goodbye"   














