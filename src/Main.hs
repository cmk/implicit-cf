module Main where



import System.Environment
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL


import qualified MatrixFactorization as MF
import qualified KNearestNeighbors as KN
import qualified Bias as B
import Utils

import Control.Monad (liftM2)

import Text.CSV 
import Data.List (intercalate)
import Data.Foldable (maximumBy)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

import Debug.Trace

import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM



main :: IO ()
--main = execute kNearestNeighbors "data/trx1.aa" "data/trx1.ab" 
--main = execute kNearestNeighbors "data/trx_data.csv" "data/trx1.ab" 
main = execute score "data/recommend_1-ALSshort.csv" "data/trx1.ab"
--main = execute score "data/recommend_1-KNN.csv" "data/trx1.ab"
--main = test "data/trx_data.csv" "data/recommend_1.csv"

--main = convert "data/trx1train.csv" "data/trx1_unbatched.csv"



{-
execute :: String -> FilePath -> IO ()
execute algorithm filePath = do
  input <- readFile filePath
  let csv = parseCSV filePath input
  either print print csv
  --either print (dispatch' algorithm) csv

check csv = print $ head $ countVectorizer csv



-}


execute :: ([Record] -> [Record] -> IO ()) -> FilePath -> FilePath -> IO ()
execute function trainFile testFile = do
  trainData <- readFile trainFile
  testData <- readFile testFile
  let trainCsv = parseCSV trainFile trainData
      testCsv = parseCSV testFile testData
      allCsv = liftM2 (,) trainCsv testCsv
  either print (uncurry function) allCsv


convert :: FilePath -> FilePath -> IO ()
convert inFilePath outFilePath = let
  header = ["CustomerID", "StockCode", "Quantity"]
  transform csv = writeFile outFilePath $ printCSV
                                        $ (header:)
                                        $ toRecords
                                        $ dataSet
                                        $ countVectorizer csv
  in do
    input <- readFile inFilePath
    let csv = parseCSV inFilePath input
    either print transform csv

    
kNearestNeighbors :: [Record] -> [Record] -> IO ()
kNearestNeighbors train test = let
  trainData = map KN.avg $ countVectorizer train      ----MOVE TO KNN FILE!!
  testData = map (read . head) $ tail $ init test :: [User] --works on holdout trx data and test data
  --users = map fst testData
  header = ["customerId", "recommendedProducts"]
  recs = KN.recommend trainData testData
  out = map (intercalate "|") $ (map . map) show recs
  glue a b = (show a) : b : []
  out' = zipWith glue testData out
  in writeFile "data/recommend_1-KNN.csv" $  printCSV $ header:out'
  


score :: [Record] -> [Record] -> IO ()
score recommended purchased = let
  purchases = map parseToTuple $ tail $ init purchased    --MOVE HEAD , TAIL OUT OF countV
  recs = (map . map) fst $ map M.toList
                         $ map snd 
                         $ map parseToTuple
                         $ tail $ init recommended
  --users = map fst purchases
  userTx = map snd purchases
  in do 
    putStrLn $ "score: " ++ (show $ scoreList userTx recs)
    putStrLn $ "nrecs: " ++ (show $ length recs)
  
scoreList :: [ItemRatingMap] -> [[Item]] -> Int
scoreList userTx recs = let
  items = zip userTx recs
  in sum $ map (uncurry scoreOne) items
  
scoreOne :: ItemRatingMap -> [Item] -> Int
--scoreOne purchases items | trace (show $ length items) False = undefined
scoreOne purchases items = let
  out = or [ M.member i purchases | i <- items]
  in case out of
   True -> 1
   False -> 0



getParams :: DataSet -> (Int, Int)
getParams dataSet = let
  max1 (a,_,_) (a',_,_) = compare a a'
  max2 (_,b,_) (_,b',_) = compare b b'
  (maxUser,_,_) = maximumBy max1 dataSet
  (_,maxItem,_) = maximumBy max2 dataSet
  in (maxUser+1, maxItem+1)


toRecords :: DataSet -> [Record]
toRecords dataSet = let
  toList (a,b,c) = [a,b,round c] 
  in V.toList $ (fmap . fmap) show $ fmap toList dataSet
  
dataSet :: [UserRatings] -> DataSet
dataSet users = let
  tuple a (b,c) = (a,b,c)
  dataPoints (user,ratings) = map (tuple user) $ M.toList ratings
  in V.fromList $ users >>= dataPoints
  
countVectorizer :: [Record] -> [UserRatings]
countVectorizer csv = let
  purchases = map parseToTuple $ tail
                               $ init csv --stripping header and trailing line
  in M.toList $ M.fromListWith (M.unionWith (+)) purchases
  

parseToTuple :: Record -> UserRatings
parseToTuple record = let
  read' r = maybe (-1) id (readMaybe r :: Maybe Int)
  --read' r = read r :: Int
  name = read' $ head record 
  items = map read' $ splitOn "|" $ head $ tail record
  items' = M.fromListWith (+) $ map (\k -> (k,1.0)) items
  in (name, items')     --FIX THIS IS STUPD


testKNearestNeighbors :: [Record] -> [Record] -> IO ()
testKNearestNeighbors trainCsv testCsv = let
  trainData = map KN.avg $ countVectorizer trainCsv
  testData = map parseToTuple $ init testCsv
  users = map fst testData
  userTx = map snd testData
  recs = KN.recommend trainData users
  in do 
    putStrLn $ "score: " ++ (show $ scoreList userTx recs)
    putStrLn $ "nrecs: " ++ (show $ length recs)
    

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

{-

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
