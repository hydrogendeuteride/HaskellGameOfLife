import Data.Set as Set
import Data.List
import Control.Concurrent

type Pos = (Int, Int)
type Board = Set Pos    --set can check double indexing easily 

row = 30
col = 50

--initial data parsing
bData::[Char]
bData = "\
    \..................................................\
    \..................................................\
    \..................................................\
    \...............................X..................\
    \.............................X.X..................\
    \...................XX......XX............XX.......\
    \..................X...X....XX............XX.......\
    \.......XX........X.....X...XX.....................\
    \.......XX........X...X.XX....X.X..................\
    \.................X.....X.......X..................\
    \..................X...X...........................\
    \...................XX.............................\
    \..................................................\
    \..................................................\
    \..................................................\
    \......XX..........................................\
    \......XX..........................................\
    \........XX........................................\
    \........XX........................................\
    \..................................................\
    \..................................................\
    \.......................X..........................\
    \........................X.........................\
    \......................XXX.........................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \..................................................\
    \...............................................…"

pos1D :: [Char] -> [Int]
pos1D = elemIndices 'X' -- find position of 'X'

pos1DT2D :: [Int] -> [Pos]
pos1DT2D zs = [(z `mod` col, z `div` col) | z <- zs]  --change position type to Int -> (Int, Int)

pos2Set :: [Pos] -> Board
pos2Set = Set.fromList 

parser:: Board
parser = Set.fromList (pos1DT2D (elemIndices 'X' bData)) 

-- game processing code

isAlive :: Board -> Pos -> Bool
isAlive b p = member p b

inBoard :: Int -> Int -> Bool
inBoard x y = (x `elem` [0..col]) && ( y `elem` [0..row])  -- check cell is in game processing area

neighbs::Pos -> Board
neighbs (x, y) = Set.fromList(Prelude.filter (uncurry inBoard)
    [(x + x', y + y') | x' <- [-1..1], y' <- [-1..1], x' /= 0 || x' - y' /= 0])  -- neighbor cell check,   x'/= 0 -> y= any ,, x' = 0 -> y /= 0 

liveNeighbs :: Board -> Pos -> Int
liveNeighbs b p = Set.size (Set.filter (isAlive b) (neighbs p)) 

cell::Board -> Pos -> Bool
cell b p
    | isAlive b p = liveNeighbs b p `elem` [2, 3]               --live cell
    | otherwise = liveNeighbs b p == 3                          --dead cell birth

renderArea :: Board -> Board
renderArea b = Set.union b $ unions [neighbs x | x <-  toList b]    --calculate live cells and its neighbors to decrease required processing power

nextGen :: Board -> Board
nextGen b = Set.filter (cell b) (renderArea b)

--IO (https://github.com/rst0git/Game-of-Life-Haskell)
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x:xs) = do x
                 seqn xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC["  ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

cls :: IO ()
cls = putStr "\ESC[2J"

showCells :: Board -> IO ()
showCells b = seqn [writeAt p "O" | p <- toList b]

life :: Board -> IO()
life b = do
    cls
    showCells b
    threadDelay 300000  --0.3s time delay
    life (nextGen b)

main :: IO ()
main = life parser