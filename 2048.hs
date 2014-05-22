{-# OPTIONS_GHC -Wall -fforce-recomp -fllvm #-}

import Data.List          (transpose, mapAccumL, elemIndices)
import System.Random      (randomRIO)
import System.Environment (getArgs)
import System.IO          (hSetBuffering, BufferMode(NoBuffering), stdin)

type Board = [[Int]]
type State = (Int, Board)
data Direction = North | South | West | East | Invalid
newtype GameState = GS (State, [State])

step  :: State -> GameState -> GameState
step  (n,b) (GS (sb@(s, _), u)) = GS ((s + n,b), sb : u)

undo  :: GameState -> GameState
undo g@(GS (_,  [])) = g
undo   (GS (_,u:us)) = GS (u,us)

reset :: GameState -> GameState
reset g@(GS (_,[])) = g
reset   (GS (_,us)) = GS (last us, [])

new :: Board -> GameState
new b = GS ((0, b), [])

empty :: Board
empty = replicate 4 . replicate 4 $ 0

holes :: Board -> [(Int, Int)]
holes = concat . zipWith (zip . repeat) [0..] . map (elemIndices 0)

update :: (Int, Int) -> Int -> Board -> Board
update (i, j) v = snd . mapAccumL go1 0
    where go1  n    x = (n+1, snd $ mapAccumL go2 (n, 0) x)
          go2 (n,m) x = ((n, m+1), if n == i && m == j then v else x)

fill :: [Int] -> [Int]
fill xs = take 4 $ xs ++ repeat 0

merge :: [Int] -> (Int, [Int])
merge (x:y:xs) | x == y    = (s + sx, s : xs')
               | otherwise = (sy, x : ys')
                    where s         = 2*x
                          (sx, xs') = merge xs
                          (sy, ys') = merge (y:xs)
merge      xs  = (0, xs)

move :: Direction -> Board -> (Int, Board)
move West board = mapAccumL go 0 board
    where go n row = (n + n', fill row')
                where (n', row') = merge . filter (/=0) $ row
move South board = (n, transpose b)
    where (n, b) = move East . transpose $ board
move North board = (n, transpose b)
    where (n, b) = move West . transpose $ board
move East board = mapAccumL go 0 board
    where go n row = (n + n', reverse . fill $ row')
                where (n', row') = merge . reverse . filter (/=0) $ row
move Invalid board = (0, board)

direction :: Char -> Direction
direction 'h' = West
direction 'j' = South
direction 'k' = North
direction 'l' = East
direction  _  = Invalid

occurrence :: [Int]
occurrence = 4 : replicate 9 2

updateRand :: Board -> IO Board
updateRand b = let emptycells = holes b in do
    p <- fmap (emptycells!!) $ randomRIO (0, length emptycells - 1)
    v <- fmap (occurrence!!) $ randomRIO (0, 9)
    return $ update p v b

draw :: Int -> Board -> IO ()
draw score board = putStrLn ("\ESC[u\ESC[0JScore: " ++ show score) >>
        mapM_ (putStrLn . (foldl draw2 "\ESC[37;1m| \ESC[0m")) board
    where draw2 s x = s ++ xcol ++ replicate spc1 ' ' ++ xstr
                                ++ replicate spc2 ' ' ++ "\ESC[0m\ESC[37;1m | \ESC[0m"
              where xstr = if x == 0 then " " else show x
                    midl = 4 - length xstr
                    (spc1, spc2) = (midl `div` 2, midl - spc1)
                    xcol = case x of 2    -> "\ESC[34;2m" ; 64   -> "\ESC[35;1m"
                                     4    -> "\ESC[34;1m" ; 128  -> "\ESC[31;2m"
                                     8    -> "\ESC[36;2m" ; 256  -> "\ESC[31;1m"
                                     16   -> "\ESC[36;1m" ; 512  -> "\ESC[32;2m"
                                     32   -> "\ESC[35;2m" ; 1024 -> "\ESC[32;1m"
                                     2048 -> "\ESC[33;2m" ; _    -> "\ESC[33;1m"

prepare :: [String] -> IO Board
prepare (a:_) = case reads a of
                    [(b, _)] -> return . map fill $ b
                    _ -> updateRand empty >>= updateRand
prepare    _  = updateRand empty >>= updateRand

usage :: IO ()
usage = putStrLn "Usage: 2048 [board] - reload a previously saved board\n\
                 \n - new\n\
                 \r - restart       h - move left\n\
                 \s - save          j - move down\n\
                 \u - undo          k - move up\n\
                 \x - quit          l - move right\n"

play :: GameState -> IO ()
play game@(GS ((score,board),_)) = draw score board >>
        if null (holes board) && (snd . move North . snd . move West $ board) == board
        then putStr "Game Over, what next ? " >> input
        else input
    where input = getChar >>= \c -> putStr "\ESC[1D" >>
                case c of
                    'n' -> prepare [] >>= play . new
                    'r' -> play $ reset game
                    's' -> putStrLn $ show board
                    'u' -> play $ undo game
                    'x' -> putStrLn "Bye !"
                    _   -> let (points, board') = move (direction c) board
                           in if board' == board
                              then play game
                              else updateRand board' >>= \board'' -> play $ step (points, board'') game

main :: IO ()
main = hSetBuffering stdin NoBuffering >> usage >> putStr "\ESC[s" >> getArgs >>= prepare >>= play . new

-- vim: et sts=4 sw=4
