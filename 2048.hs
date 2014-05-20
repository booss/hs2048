import Data.List     (transpose, mapAccumL, elemIndices)
import System.Random (randomRIO)

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

addRandom :: Board -> IO Board
addRandom b = let e = holes b in do
    p <- fmap (e!!) $ randomRIO (0, length e - 1)
    v <- randomRIO (0.0, 1.0) :: IO Float
    return $ update p (if v < 0.9 then 2 else 4) b

display :: Board -> IO ()
display = mapM_ $ putStrLn . (foldl disp2 "\ESC[37;1m| \ESC[0m")
    where disp2 s x = s ++ xcol ++ replicate spc1 ' ' ++ xstr ++ replicate spc2 ' ' ++ "\ESC[0m\ESC[37;1m | \ESC[0m"
              where xstr = if x == 0 then " " else show x
                    midl = 4 - length xstr
                    (spc1, spc2) = (midl `div` 2, midl - spc1)
                    xcol = case x of 2    -> "\ESC[36;1m" ; 64   -> "\ESC[36m"
                                     4    -> "\ESC[34;1m" ; 128  -> "\ESC[34m"
                                     8    -> "\ESC[35;1m" ; 256  -> "\ESC[35m"
                                     16   -> "\ESC[31;1m" ; 512  -> "\ESC[31m"
                                     32   -> "\ESC[32;1m" ; 1024 -> "\ESC[32m"
                                     _    -> "\ESC[38m"

main :: IO ()
main = putStr "\ESC[s" >> addRandom emptyBoard >>= addRandom >>= play
    where play board = putStr "\ESC[1K\ESC[1D\ESC[u" >> display board
            >> if null (holes board) && ((move North . move West) board == board)
               then putStrLn "Game Over !"
               else getChar >>= \c ->
                        if c == 'x'
                        then putStr "\ESC[1K\ESC[1D" >> return ()
                        else let board' = move (direction c) board
                             in if board' == board
                                then play board
                                else addRandom board' >>= play
