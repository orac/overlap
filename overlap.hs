import System.IO
import System.Environment
import Data.List
import qualified Data.ByteString.Lazy.Char8 as L

readWordList :: FilePath -> IO [L.ByteString]
readWordList file = do
    content <- L.readFile file
    return $ L.split '\n' content

sortedIntersect :: [L.ByteString] -> [L.ByteString] -> [L.ByteString]
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
        mapM_ L.putStrLn $ if null wordLists' then [] else foldr1 sortedIntersect wordLists'
