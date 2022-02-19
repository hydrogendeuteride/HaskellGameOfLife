import Data.Set as Set

row = 30
col = 50

type Pos = (Int, Int)
type Board = Set Pos

isAlive :: Board -> Pos -> Bool
isAlive b p = member p b

inBoard :: Int -> Int -> Bool
inBoard x y = (x `elem` [0..row]) && ( y `elem` [0..col])

neighbs::Pos -> Board
neighbs (x, y) = Set.fromList(Prelude.filter (uncurry inBoard)
    [(x + x', y + y') | x' <- [-1..1], y' <- [-1..1], x' /= 0 || x' -y' /= 0])

liveNeighbs :: Board -> Pos -> Int
liveNeighbs b p = Set.size (Set.filter (isAlive b) (neighbs p))

cell::Board -> Pos -> Bool
cell b p
    | isAlive b p = liveNeighbs b p `elem` [2, 3]
    | otherwise = liveNeighbs b p == 3

renderArea :: Board -> Board
renderArea b = union b $ unions [neighbs x | x <-  toList b]

nextGen :: Board -> Board
nextGen b = Set.filter (cell b) (renderArea b)

--heavily referenced: https://gist.github.com/ihabunek/81e7da0c705689fe743a
--IO
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
    life (nextGen b)