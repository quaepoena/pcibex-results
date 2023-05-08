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

average :: [Int] -> Double
average values = (/) (fromIntegral (sum values))
                     (fromIntegral (length values))

printAverage :: (Int, Double) -> String
printAverage (item, avg) = show item ++ "," ++ show avg

main = do
  (inputFile:_) <- getArgs
  contents <- readFile inputFile

  let results = map toResult
              . filter relevantLines
              . map (splitOn ",")
              $ lines contents

      resultsByUser = Map.filter (\ x -> length x == 24)
                    . Map.fromListWith (++)
                    $ map (\ (Result user item value) -> (user, [(item, value)])) results

      resultsByItem = Map.fromListWith (++)
                    . map (\ (item, value) -> (item, [value]))
                    $ Map.foldl (++) [] resultsByUser

      averagePerItem = Map.toAscList $ Map.map average resultsByItem

  mapM_ putStrLn $ ["item,average"] ++ map printAverage averagePerItem
