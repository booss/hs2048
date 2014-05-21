{-# OPTIONS_GHC -Wall -fforce-recomp -fllvm #-}

import Data.List          (transpose, mapAccumL, elemIndices)
import System.Random      (randomRIO)
import System.Environment (getArgs)
import System.IO          (hSetBuffering, BufferMode(NoBuffering), stdin)

type Board = [[Int]]
data Direction = North | South | West | East | Invalid

data GameState = GameState
           { gBoard :: Board
           , gScore :: Int
           , gUndo  :: [(Int, Board)]
           }

emptyBoard :: Board
emptyBoard = replicate 4 . replicate 4 $ 0

fill :: [Int] -> [Int]
fill xs = take 4 $ xs ++ repeat 0

merge :: [Int] -> (Int, [Int])
merge (x:y:xs) | x == y    = let s = 2*x
                                 (s', xs') = merge xs
                             in (s + s', s : xs')
               | otherwise = let (s, xs') = merge (y : xs)
                             in (s, x : xs')
merge      xs  = (0, xs)

move :: Direction -> Board -> (Int, Board)
move West board = mapAccumL go 0 board
    where go n row = let (n', r') = merge . filter (/=0) $ row
                     in (n + n', fill r')
move South board = let (s, b) = move East (transpose board)
                   in (s, transpose b)
move North board = let (s, b) = move West (transpose board)
                   in (s, transpose b)
move East board = mapAccumL go 0 board
    where go n row = let (n', r') = merge . reverse . filter (/=0) $ row
                     in (n + n', reverse . fill $ r')
move Invalid board = (0, board)

direction :: Char -> Direction
direction 'h' = West
direction 'j' = South
direction 'k' = North
direction 'l' = East
direction  _  = Invalid

holes :: Board -> [(Int, Int)]
holes = concat . zipWith (zip . repeat) [0..] . map (elemIndices 0)

update :: (Int, Int) -> Int -> Board -> Board
update (i, j) v = snd . mapAccumL go1 0
    where go1  n    x = (n+1, snd $ mapAccumL go2 (n, 0) x)
          go2 (n,m) x = ((n, m+1), if n == i && m == j then v else x)

occurrence :: [Int]
occurrence = 4 : replicate 9 2

addRandom :: Board -> IO Board
addRandom b = let emptycells = holes b in do
    p <- fmap (emptycells!!) $ randomRIO (0, length emptycells - 1)
    v <- fmap (occurrence!!) $ randomRIO (0, 9)
    return $ update p v b

draw :: GameState -> IO ()
draw gs = putStrLn ("\ESC[KScore: " ++ show (gScore gs))
            >> mapM_ (putStrLn . (foldl disp2 "\ESC[37;1m| \ESC[0m")) (gBoard gs)
    where disp2 s x = s ++ xcol ++ replicate spc1 ' ' ++ xstr
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

buildBoard :: [String] -> IO Board
buildBoard (a:_) = case reads a of
                    [(b, _)] -> return . map fill $ b
                    _ -> addRandom emptyBoard >>= addRandom
buildBoard    _  = addRandom emptyBoard >>= addRandom

display :: String -> IO ()
display s = putStr $ "\ESC[1K\ESC[1D" ++ s

usage :: IO ()
usage = putStrLn "Usage: 2048 [board] - reload a previously saved board\n\
                 \n - new\n\
                 \r - restart       h - move left\n\
                 \s - save          j - move down\n\
                 \u - undo          k - move up\n\
                 \x - quit          l - move right\n"

undo :: GameState -> GameState
undo g@GameState { gUndo = [] }          = g
undo g@GameState { gUndo = ((s, b):us) } = g { gBoard = b, gScore = s, gUndo = us }

reset :: GameState -> GameState
reset g@GameState { gUndo = [] } = g
reset g@GameState { gUndo =  u } = g { gBoard = snd . last $ u, gScore = 0, gUndo = [] }

step :: GameState -> Int -> Board -> GameState
step g@GameState { gBoard = b, gScore = s, gUndo = u } points board =
        g { gBoard = board, gScore = s + points, gUndo = (s,b):u }

new :: Board -> GameState
new board = GameState { gBoard = board, gScore = 0, gUndo = [] }

loop :: GameState -> IO ()
loop game = let board = gBoard game in display "\ESC[u" >> draw game >>
        if null (holes board) && (snd . move North . snd . move West $ board) == board
        then putStrLn "Game Over !"
        else getChar >>= \c ->
                case c of
                    'n' -> display "\ESC[u" >> buildBoard [] >>= loop . new
                    'r' -> display "\ESC[u" >> loop (reset game)
                    's' -> display $ show board ++ "\n"
                    'u' -> loop (undo game)
                    'x' -> display "Bye !\n"
                    _   -> let (points, board') = move (direction c) board
                           in if board' == board
                              then loop game
                              else addRandom board' >>= loop . (step game points)

play :: IO ()
play = getArgs >>= buildBoard >>= loop . new

main :: IO ()
main = hSetBuffering stdin NoBuffering >> usage >> putStr "\ESC[s" >> play

-- vim: et sts=4 sw=4
