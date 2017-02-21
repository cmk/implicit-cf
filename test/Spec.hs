module Main (main) where

import Data.List (delete)
import Test.Hspec

import qualified Data.Map.Strict as M
import qualified KNearestNeighbors as KN

import Exec

main :: IO ()
main = spec
--main = execute (kNearestNeighbors "data/recommend_1-KNN.csv") "data/trx_data.csv" "data/trx1.ab"
--main = execute score "data/recommend_1-KNN.csv" "data/trx1.ab"
--main = convert "data/trx1train.csv" "data/trx1_unbatched.csv"

spec :: IO ()
spec = hspec $ do
  describe "KNN" $ do
    
    it "jaccard should correctly compute" $ do
      (KN.jaccard a emptyUser) `shouldBe` 0.0
      (KN.jaccard a i) `shouldBe` 0.0
      (KN.jaccard a h) `shouldBe` 0.5
      (KN.jaccard a a) `shouldBe` 1.0
      
    it "sortNeighbors for a" $ do
      (map snd $ KN.sortNeighbors allSamples a) `shouldBe` aNeighbors
      
    it "getRatings for i" $ do
      (map fst $ KN.getRatings 10 allSamples i) `shouldBe` iRatings


allSamples = [a, b, c, d, e, f, g, h, i]

aNeighbors = [a, g, c, f, d, h, b, e, i]

iRatings = [7, 1, 2, 6, 9, 0, 3, 4, 8, 5]

emptyUser = (-1, M.empty)

a = (,) 0 $ M.fromList [(0, 3.0), 
                        (1, 2.0),
                        (2, 4.0),
                        (3, 5.0),
                        (4, 1.0),
                        (5, 2.0),
                        (6, 2.0)]
                        
b = (,) 1 $ M.fromList [(0, 2.0),
                        (1, 3.0),
                        (7, 4.0),
                        (8, 2.0),
                        (4, 3.0),
                        (6, 3.0)]
                   
c = (,) 2 $ M.fromList [(0, 5.0),
                        (1, 1.0),
                        (7, 1.0),
                        (2, 3.0),
                        (3, 5.0),
                        (4, 1.0)]

d = (,) 3 $ M.fromList [(0, 3.0),
                        (1, 4.0),
                        (7, 4.0),
                        (3, 3.0),
                        (4, 4.0),
                        (9, 4.0),
                        (6, 2.0)]

e = (,) 4 $ M.fromList [(1, 4.0),
                        (7, 1.0),
                        (2, 4.0),
                        (9, 4.0),
                        (6, 1.0)]

f = (,) 5 $ M.fromList [(1, 4.0),
                        (7, 4.0),
                        (2, 5.0),
                        (3, 5.0),
                        (4, 4.0),
                        (9, 4.0),
                        (6, 4.0)]

g = (,) 6 $ M.fromList [(0, 5.0),
                        (1, 2.0),
                        (2, 3.0),
                        (3, 5.0),
                        (4, 4.0),
                        (9, 5.0)]

h = (,) 7 $ M.fromList [(0, 3.0),
                        (2, 5.0),
                        (3, 4.0),
                        (4, 2.0),
                        (9, 3.0)]

i = (,) 8 $ M.fromList [(7, 1.0)]
