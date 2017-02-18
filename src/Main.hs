module Main where



import System.Environment
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL


import qualified MatrixFactorization as MF
import qualified Bias as B
import Utils


import Text.CSV (parseCSV, Record)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import qualified Data.Map as M

import qualified UserBased as UB


main :: IO ()
main = do
  let fileName = "data/test.csv"
  input <- readFile fileName
  let csv = parseCSV fileName input
  either print doWork csv

doWork :: [Record] -> IO ()
doWork csv = let
  users = map avg $ M.elems $ M.fromListWith combine $ map parseToTuple $ tail $ init csv
  user = avg $ UserRatings (-1) $ M.fromList [(8,2),(48,1)]
  closests = take 30 $ UB.sortNeighbours UB.cosine user users
  recs = UB.recommend UB.cosine user users
  r = UB.ratings $ UB.findUser (snd $ head closests) users :: UserRatingMap
  in do
    --print users
    print closests
    print recs
    print user
    print r

avg :: UserRatings -> UserRatings
avg (UserRatings user ratings) = let
  sum = M.foldl' (+) 0.0 ratings
  ratings' = M.map (\r -> r/sum) ratings
  in UserRatings user ratings'

doWork' :: [Record] -> IO ()
doWork' csv = let
  csv' = tail $ init csv
  read' r = maybe (-1) id (readMaybe r :: Maybe Int) 
  items record = map read' $ splitOn "|" $ head $ tail record
  in print $ map items csv'

combine :: UserRatings -> UserRatings -> UserRatings
combine (UserRatings a1 b1) (UserRatings a2 b2) =
  let new = M.unionWith (+) b1 b2 :: UserRatingMap
  in UserRatings a1 new

parseToTuple :: [String] -> (Int, UserRatings)
parseToTuple record = let
  read' r = maybe (-1) id (readMaybe r :: Maybe Int)
  --read' r = read r :: Int
  name = read' $ head record 
  items = map read' $ splitOn "|" $ head $ tail record
  items' = M.fromListWith (+) $ map (\k -> (k,1)) items
  in (name, UserRatings name items')

{-

import Data.Csv

main = do
  (command:argList) <- getArgs
  dispatch command argList

dispatch :: String -> [String] -> IO ()
dispatch "bias" = bias 
dispatch "mf" = matrixFactorization
dispathc _ = bias

--matrixFactorization ["data/u1formatted.base","data/u1formatted.test"]
matrixFactorization :: [String] -> IO ()
matrixFactorization (base:test:xs) =  do
  putStrLn "Start userbased recommender"
  (traindata, testdata) <- loadData base test
  m <- MF.model traindata
  let bm = B.model traindata
  putStrLn $ "Mean Absolute Error: " ++ (show $ mae (MF.predict bm m) testdata)  

toVec :: Either String DataSet -> DataSet
toVec (Left err) = error err
toVec (Right v) = v

mae :: (Int -> Int -> Double) -> DataSet -> Double
mae p v = (V.sum $ errors p v) / (fromIntegral (V.length v))

errors :: ( Int -> Int -> Double ) -> DataSet -> V.Vector Double
errors p v = V.filter (\x -> not $ isNaN x) $ V.map (\(u, i, r) -> abs (r - p u i)) v
  where diff (u, i, r) = abs (r - p u i)

bias (base:test:xs) = do
  putStrLn "Start bias recommender"
  putStrLn "Loading Data"
  (modelv, testv) <- loadData base test
  putStrLn $ "Mean Absolute Error: " ++ (show $ mae (B.predict $ B.model modelv) testv)  

loadData :: String -> String -> IO (DataSet, DataSet) 
loadData basefile testfile = do
  c1 <- BL.readFile basefile
  c2 <- BL.readFile testfile
  let baseData = decode NoHeader c1 :: Either String (DataSet)
  let testData = decode NoHeader c2 :: Either String (DataSet)
  let modelv =  toVec baseData
  let testv = toVec testData
  return (modelv, testv)



-}
