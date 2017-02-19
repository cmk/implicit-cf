module Main where



import System.Environment
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL


import qualified MatrixFactorization as MF
import qualified KNearestNeighbors as KN
import qualified Bias as B
import Utils


import Text.CSV (parseCSV, Record)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Text.Read (readMaybe)
import Data.Foldable (maximumBy)


import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM



main :: IO ()
main = do
  let fileName = "data/test3.csv"
  input <- readFile fileName
  let csv = parseCSV fileName input
  either print kNearestNeighbors csv

matrixFactorization :: [Record] -> IO ()
matrixFactorization csv = do
  putStrLn "Start factorization-based recommender"
  let users = load csv
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

kNearestNeighbors :: [Record] -> IO ()
kNearestNeighbors csv = let
  trainData = map KN.avg $ load csv
  testData = [0..11]
  recs = KN.recommend trainData testData
  --r = KN.ratings $ KN.findUser (snd $ head closests) users
  in do
    putStrLn "Start knn-based recommender"
    putStrLn $ intercalate "\n" $ map show recs

load :: [Record] -> [UserRatings]
load csv = M.elems $ M.fromListWith combine $ map parseToTuple $ tail $ init csv
  
dataSet :: [UserRatings] -> DataSet
dataSet users = let
  tuple a (b,c) = (a,b,c)
  dataPoints (user,ratings) = map (tuple user) $ M.toList ratings
  in V.fromList $ users >>= dataPoints
  
combine :: UserRatings -> UserRatings -> UserRatings
combine (a1, b1) (a2, b2) =
  let new = M.unionWith (+) b1 b2 :: ItemRatingMap
  in (a1, new)

parseToTuple :: [String] -> (Int, UserRatings)
parseToTuple record = let
  read' r = maybe (-1) id (readMaybe r :: Maybe Int)
  --read' r = read r :: Int
  name = read' $ head record 
  items = map read' $ splitOn "|" $ head $ tail record
  items' = M.fromListWith (+) $ map (\k -> (k,1.0)) items
  in (name, (name, items'))

getParams :: DataSet -> (Int, Int)
getParams dataSet = let
  max1 (a,_,_) (a',_,_) = compare a a'
  max2 (_,b,_) (_,b',_) = compare b b'
  (maxUser,_,_) = maximumBy max1 dataSet
  (_,maxItem,_) = maximumBy max2 dataSet
  in (maxUser+1, maxItem+1)



              
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
