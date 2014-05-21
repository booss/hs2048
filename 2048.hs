{-# OPTIONS_GHC -Wall -fforce-recomp -fllvm #-}

import Data.List          (transpose, mapAccumL, elemIndices)
import System.Random      (randomRIO)
import System.Environment (getArgs)
import System.IO          (hSetBuffering, BufferMode(NoBuffering), stdin)

type Board = [[Int]]
data Direction = North | South | West | East | Invalid

emptyBoard :: Board
emptyBoard = replicate 4 . replicate 4 $ 0

fill :: [Int] -> [Int]
fill xs = take 4 $ xs ++ repeat 0

merge :: [Int] -> [Int]
merge (x:y:xs) | x == y    = 2*x : merge xs
               | otherwise =   x : merge (y : xs)
merge      xs  = xs

move :: Direction -> Board -> Board
move West    = map $ fill . merge . filter (/=0)
move South   = transpose . move East . transpose
move North   = transpose . move West . transpose
move East    = map $ reverse . fill . merge . reverse . filter (/=0)
move Invalid = \b -> b

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

draw :: Board -> IO ()
draw = mapM_ $ putStrLn . (foldl disp2 "\ESC[37;1m| \ESC[0m")
    where disp2 s x = s ++ xcol ++ replicate spc1 ' ' ++ xstr ++ replicate spc2 ' ' ++ "\ESC[0m\ESC[37;1m | \ESC[0m"
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
buildBoard (a:_) = return . map fill . read $ a
buildBoard    _  = addRandom emptyBoard >>= addRandom

display :: String -> IO ()
display s = putStr $ "\ESC[1K\ESC[1D" ++ s

usage :: IO ()
usage = putStrLn "Usage: 2048 [board]    h - move left\n\
                 \r - restart            j - move down\n\
                 \s - save game          k - move up\n\
                 \x - quit               l - move right\n"

play :: IO ()
play = getArgs >>= buildBoard >>= loop
    where loop board = display "\ESC[u" >> draw board >>
                if null (holes board) && ((move North . move West) board == board)
                then putStrLn "Game Over !"
                else getChar >>= \c ->
                        case c of
                            'x' -> display ""
                            'r' -> display "\ESC[u" >> play
                            's' -> display $ show board ++ "\n"
                            _   -> let board' = move (direction c) board
                                   in if board' == board
                                      then loop board
                                      else addRandom board' >>= loop

main :: IO ()
main = hSetBuffering stdin NoBuffering >> usage >> putStr "\ESC[s" >> play

-- vim: et sts=4 sw=4
