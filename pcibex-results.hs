import Data.List.Split(splitOn)
import System.Environment(getArgs)
import System.IO(readFile)

import qualified Data.Map.Strict as Map

-- User ID, item number, value given.
data Result = Result String Int Int

relevantLines :: [String] -> Bool
relevantLines result = length result == 13 &&
                       result !! 5 == "experimental-trial" &&
                       result !! 7 == "Scale" &&
                       result !! 10 /= "NA"

toResult :: [String] -> Result
toResult line = Result (line !! 0 ++ "-" ++ line !! 1)
                (read (line !! 3) :: Int)
                (read (line !! 10) :: Int)

bar'' :: String -> (Int, Int) -> (String, Int, Int)
bar'' user (item, value) = (user, item, value)

bbar' :: (String, [(Int, Int)]) -> [(String, Int, Int)]
bbar' (_, []) = []
bbar' (user, (x:xs)) = bar'' user x : bbar' (user, xs)

bar :: [(String, [(Int, Int)])] -> [(String, Int, Int)]
bar [] = []
bar (x:xs) = bbar' x ++ bar xs

printResult :: (String, Int, Int) -> String
printResult (user, item, value) = show user ++ "," ++ show item ++ "," ++ show value

main = do
  (inputFile:_) <- getArgs
  contents <- readFile inputFile

  let results = map toResult
              . filter relevantLines
              . map (splitOn ",")
              $ lines contents

      -- Remove users that didn't complete the experiment.
      resultsByUser = Map.filter (\ x -> length x == 24)
                    . Map.fromListWith (++)
                    $ map (\ (Result user item value) -> (user, [(item, value)])) results

      validResults = bar $ Map.toList resultsByUser

  mapM_ putStrLn $ ["user,item,value"] ++ map printResult validResults
