module Main where



import System.Environment
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL


import qualified MatrixFactorization as MF
import qualified KNearestNeighbors as KN
import qualified Bias as B
import Utils

import Control.Monad (liftM2)

import Text.CSV (parseCSV, Record)
import Data.List (intercalate)
import Data.Foldable (maximumBy)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

import Debug.Trace

import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM



main :: IO ()
--main = execute "knn" "data/test3.csv"
main = test "knn" "data/trx1.aa" "data/trx1.ab"



execute :: String -> FilePath -> IO ()
execute algorithm filePath = do
  input <- readFile filePath
  let csv = parseCSV filePath input
  either print (dispatch' algorithm) csv

test :: String -> FilePath -> FilePath -> IO ()
test algorithm trainFile testFile = do
  trainData <- readFile trainFile
  testData <- readFile testFile
  let trainCsv = parseCSV trainFile trainData
      testCsv = parseCSV testFile testData
      allCsv = liftM2 (,) trainCsv testCsv
  either print (uncurry kNearestNeighbors) allCsv


dispatch' :: String -> [Record] -> IO ()
--dispatch' "knn" = kNearestNeighbors
dispatch' "mf" = matrixFactorization


  

kNearestNeighbors :: [Record] -> [Record] -> IO ()
kNearestNeighbors trainCsv testCsv = let
  trainData = map KN.avg $ countVectorizer trainCsv
  testData = map parseToTuple $ init testCsv
  users = map fst testData
  userTx = map snd testData
  recs = KN.recommend trainData users
  in do 
    putStrLn $ "score: " ++ (show $ score userTx recs)
    putStrLn $ "nrecs: " ++ (show $ length recs)


score :: [ItemRatingMap] -> [[Item]] -> Int
score userTx recs = let
  items = zip userTx recs
  in sum $ map (uncurry scoreOne) items
  
scoreOne :: ItemRatingMap -> [Item] -> Int
scoreOne purchases items | trace (show $ length items) False = undefined
scoreOne purchases items = let
  out = or [ M.member i purchases | i <- items]
  in case out of
   True -> 1
   False -> 0

{-
[2,55,69,84,17,21,47,67,97,135]
[0]
[0]
[0]
[124,44,64,101,133,149,180,219,13,290]
[15,21,109,34,211,1,13,226,234,277]
[14,133,237,113,7,101]
[240,60,78,95,141,296,297,289,2,35]
[93,259,89,68,0,1,2,16,25,31]
[6,16,66,242,259,37,81,9,130,142]
[94,247,2,0,89,1,8,93,200,233]
[124,37,1,51,63,114,208,224,225,30]
[40,121,150,175,130,148,14,1,3,257]
[48,8,14,16,79,44,2,21,36,38]
[2,296,39,81,179,211,268,275,215,100]



scoreOne purchases items | trace (show items) False = undefined
scoreOne purchases items = let
  r = map fst $ M.toList purchases
  out = [1.0 | i <- r, elem i items]
  n = fromIntegral $ length r :: Double
  in sum out / n

kNearestNeighbors :: [Record] -> IO ()
kNearestNeighbors csv = let
  trainData = map KN.avg $ countVectorizer csv
  testData = [0..11]
  recs = KN.recommend trainData testData
  --r = KN.ratings $ KN.findUser (snd $ head closests) users
  in do
    putStrLn "Start knn-based recommender"
    putStrLn $ intercalate "\n" $ map show recs
-}

    
matrixFactorization :: [Record] -> IO ()
matrixFactorization csv = do
  putStrLn "Start factorization-based recommender"
  let users = countVectorizer csv
      trainData = dataSet users
      testData = [0..11]
      --bm = B.model trainData
  --(trainData, testData) <- loadData base test
  putStrLn $ "datasetSize: " ++ (show $ V.length trainData)
  putStrLn $ "nUsers, nItems " ++ (show $ getParams trainData)
  m <- MF.model trainData
  putStrLn ""
  --print m
  putStrLn $ MF.showMat $ MF.getR m
  putStrLn $ intercalate "\n" $ map show $ MF.recommend m testData
  --putStrLn $ MF.showMat $ MF.getP m
  --putStrLn $ MF.showMat $ MF.getQ m
  
  --putStrLn $ "Mean Absolute Error: " ++ (show $ mae (MF.predict bm m) testData)  

getParams :: DataSet -> (Int, Int)
getParams dataSet = let
  max1 (a,_,_) (a',_,_) = compare a a'
  max2 (_,b,_) (_,b',_) = compare b b'
  (maxUser,_,_) = maximumBy max1 dataSet
  (_,maxItem,_) = maximumBy max2 dataSet
  in (maxUser+1, maxItem+1)


countVectorizer :: [Record] -> [UserRatings]
countVectorizer csv = let
  purchases = map parseToTuple $ tail $ init csv --stripping header and trailing line
  in M.toList $ M.fromListWith (M.unionWith (+)) purchases
  
dataSet :: [UserRatings] -> DataSet
dataSet users = let
  tuple a (b,c) = (a,b,c)
  dataPoints (user,ratings) = map (tuple user) $ M.toList ratings
  in V.fromList $ users >>= dataPoints
  


parseToTuple :: Record -> UserRatings
parseToTuple record = let
  read' r = maybe (-1) id (readMaybe r :: Maybe Int)
  --read' r = read r :: Int
  name = read' $ head record 
  items = map read' $ splitOn "|" $ head $ tail record
  items' = M.fromListWith (+) $ map (\k -> (k,1.0)) items
  in (name, items')     --FIX THIS IS STUPD




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

toVec :: Either String DataSet -> DataSet
toVec (Left err) = error err
toVec (Right v) = v

-}
