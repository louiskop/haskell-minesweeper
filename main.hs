
import System.IO

type Coord = (Int, Int)

-- create empty board
createBoard :: Int -> Int -> [[Char]]
createBoard rows cols = replicate rows $ replicate cols '▯'

-- Convert list to Coordinate
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
    putStrLn ""
    mapM_ putStrLn board
    gameLoop board


main :: IO()
main = do

    putStrLn "Minesweeper in haskell"  
    
    -- Create board 
    board <- return $ createBoard 10 10

    -- run the game loop 
    gameLoop board

    putStr "Goodbye"   














