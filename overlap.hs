import System.IO
import System.Environment
import Data.List
import qualified Data.Text as T

readWordList :: FilePath -> IO [T.Text]
readWordList file = do
    content <- readFile file
    let list = lines content in
        return $ sort $ map (T.toUpper . T.pack) list

sortedIntersect :: Ord a => [a] -> [a] -> [a]
sortedIntersect (x:xs) (y:ys) = case compare x y of
                                  LT -> sortedIntersect xs (y:ys)
                                  GT -> sortedIntersect (x:xs) ys
                                  EQ -> x : sortedIntersect xs ys
sortedIntersect _ _ = []

main :: IO ()
main = do
    files <- getArgs
    wordLists <- mapM readWordList files
    let wordLists' = map sort wordLists in
        mapM_ (putStrLn . T.unpack) $ if null wordLists' then [] else foldl1 sortedIntersect wordLists'
