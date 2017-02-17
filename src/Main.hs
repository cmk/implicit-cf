
module Main where

import Data.Csv

import Bias as B
import System.Environment
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.MultiMap as MM
import qualified Data.Map as M
import qualified MatrixFactorization as MF

type Rating = (Int, Int, Double)
type User = Int
type Item = Int

main = do
  (command:argList) <- getArgs
  dispatch command argList

dispatch :: String -> [String] -> IO ()
dispatch "bias" = bias 
dispatch "mf" = matrixFactorization
dispathc _ = bias

matrixFactorization :: [String] -> IO ()
matrixFactorization (base:test:xs) =  do
  putStrLn "Start userbased recommender"
  (traindata, testdata) <- loadData base test
  m <- MF.model traindata
  let bm = B.model traindata
  putStrLn $ "Mean Absolute Error: " ++ (show $ mae (MF.predict m bm) testdata)  

toVec :: Either String (V.Vector Rating) -> V.Vector Rating
toVec (Left err) = error err
toVec (Right v) = v

mae :: (Int -> Int -> Double) -> V.Vector Rating -> Double
mae p v = (V.sum $ errors p v) / (fromIntegral (V.length v))

errors :: ( Int -> Int -> Double ) -> V.Vector Rating -> V.Vector Double
errors p v = V.filter (\x -> not $ isNaN x) $ V.map (\(u, i, r) -> abs (r - p u i)) v
  where diff (u, i, r) = abs (r - p u i)

bias (base:test:xs) = do
  putStrLn "Start bias recommender"
  putStrLn "Loading Data"
  (modelv, testv) <- loadData base test
  putStrLn $ "Mean Absolute Error: " ++ (show $ mae (B.predict $ B.model modelv) testv)  

loadData :: String -> String -> IO (V.Vector Rating, V.Vector Rating) 
loadData basefile testfile = do
  c1 <- BL.readFile basefile
  c2 <- BL.readFile testfile
  let baseData = decode NoHeader c1 :: Either String (V.Vector Rating)
  let testData = decode NoHeader c2 :: Either String (V.Vector Rating)
  let modelv =  toVec baseData
  let testv = toVec testData
  return (modelv, testv)
