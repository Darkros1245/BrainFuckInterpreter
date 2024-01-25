import Data.Char
import Text.Printf

main :: IO ()
main = do
    s <- getLine
    interpret s

dataLength :: Int
dataLength = 30000

interpret :: String -> IO ()
interpret s = interpret' s 0 [0 | _ <- [1..dataLength]] 0
    where
        ifMatches = matchifs s
        interpret' instructions ip _data dp
            | ip == (length instructions) = return ()
            | (instructions !! ip) == '>' = do
                interpret' instructions (moveRight ip) _data (moveRight dp)
            | (instructions !! ip) == '<' = do
                interpret' instructions (moveRight ip) _data (moveLeft dp)
            | (instructions !! ip) == '+' = do
                interpret' instructions (moveRight ip) (inc _data dp) dp
            | (instructions !! ip) == '-' = do
                interpret' instructions (moveRight ip) (dec _data dp) dp
            | (instructions !! ip) == '.' = do
                _ <- output _data dp
                interpret' instructions (moveRight ip) _data dp
            | (instructions !! ip) == ',' = do
                _data' <- (input _data dp)
                interpret' instructions (moveRight ip) _data' dp
            | (instructions !! ip) == '[' = do
                let corosponding = corospondingEnd ip ifMatches
                let ip' = if0 _data dp corosponding ip
                interpret' instructions (moveRight ip') _data dp
            | (instructions !! ip) == ']' = do
                let corosponding = corospondingStart ip ifMatches
                let ip' = ifn0 _data dp corosponding ip
                interpret' instructions (moveRight ip') _data dp
            | otherwise = do
                interpret' instructions (moveRight ip) _data dp

moveRight :: Int -> Int
moveRight p = p + 1

moveLeft :: Int -> Int
moveLeft p = p - 1

inc :: [Int] -> Int -> [Int]
inc _data dp = [(\(x', i') -> if i' == dp then x' + 1 else x') x | x <- (zip _data [0..])]

dec :: [Int] -> Int -> [Int]
dec _data dp = [(\(x', i') -> if i' == dp then x' - 1 else x') x | x <- (zip _data [0..])]

output :: [Int] -> Int -> IO ()
output _data dp = do
    printf "%c" (chr (_data !! dp)) -- :: IO ()
    return ()

input :: [Int] -> Int -> IO [Int]
input _data dp = do
    s <- getChar
    let s' = ord s
    return [(\(x', i') -> if i' == dp then s' else x') x | x <- (zip _data [0..])]

if0 :: [Int] -> Int -> Int -> Int -> Int
if0 _data dp corosponding ip = if (_data !! dp) == 0 then corosponding else ip

ifn0 :: [Int] -> Int -> Int -> Int -> Int
ifn0 _data dp corosponding ip = if (_data !! dp) /= 0 then corosponding else ip

matchifs :: String -> [(Int, Int)]
matchifs s = matchifs' (zip s [0..]) [] []
    where
        matchifs' [] matches iStack = matches
        matchifs' ((c, i):xs) matches iStack
            | c == '[' = matchifs' xs matches (iStack ++ [i])
            | c == ']' = matchifs' xs (matches ++ [((last iStack), i)]) (init iStack)
            | otherwise = matchifs' xs matches iStack

corospondingStart :: Int -> [(Int, Int)] -> Int
corospondingStart ip (m:ms) = if (snd m) == ip then fst m else corospondingStart ip ms

corospondingEnd :: Int -> [(Int, Int)] -> Int
corospondingEnd ip (m:ms) = if (fst m) == ip then snd m else corospondingEnd ip ms