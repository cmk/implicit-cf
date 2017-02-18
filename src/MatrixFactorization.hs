module MatrixFactorization where -- (model, predict) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U 
import qualified Data.MultiMap as MM
import qualified Data.Map as M

import qualified Numeric.SGD as S
import qualified Numeric.SGD.Dataset as D
import Numeric (showFFloat)
import Data.List (foldl')
import qualified Bias as B
import Text.Printf

import Utils

import Debug.Trace

import qualified Data.Matrix as DM


import System.Random (randomRIO)
import Control.Monad (replicateM)

type Matrices = U.Vector Double -- factor matrices P, Q^t stored in row-major order


{- notes / TODO

user Reader to encode config stuff like Matrices, BiasModel etc
fix gradP, gradQ repetition with ap or map2 or somesuch
nItems = 10 --should be max item # found +1
-}


rate = 0.25 -- initial learning rate
alpha = 0.2 -- confidence scaling factor, see Hu et al section 4
lambda = 0.0 -- Tikhonov regularization paramater

nr_iter = 150
nFeatures = 3
nUsers = 10
nItems = 10 --should be max item # found +1
fRange = [0..nFeatures-1]
--datasetSize = 494
{-
getP :: Matrices -> DM.Matrix Double
getP matrices = let
  get (item,f) = matrices U.! (itemIndex item f) 
  in DM.matrix nItems nFeatures get

getQ :: Matrices -> DM.Matrix Double
getQ matrices = let
  get (user,f) = matrices U.! (userIndex user f)
  in DM.matrix nUsers nFeatures get
-}


getP matrices = let
  l = take (nItems * nFeatures) $ U.toList matrices
  in DM.fromList nItems nFeatures l

getQ matrices = let
  l = drop (nItems * nFeatures) $ U.toList matrices
  in DM.fromList nUsers nFeatures l
  
getR matrices = let
  p = getP matrices
  q = getQ matrices
  in DM.multStd p $ DM.transpose q

showMat :: DM.Matrix Double -> String
showMat m = show $ fmap (\i -> showFFloat (Just 2) i "") m


-- | computes a prediction for the rating of user u on item i 
predict :: BiasModel -> Matrices -> User -> Item -> Rating
predict biasModel matrices user item = let
  bias = B.predict biasModel user item
  prediction = sum [(matrices U.! (itemIndex item f)) * (matrices U.! (userIndex user f))| f <- fRange]
  in bias + prediction

-- retrieve the index of an latent item feature in the parameter vector
itemIndex :: Item -> Int -> Int
--itemIndex x f | trace ("item: " ++ (show x) ++ " " ++  (show f)) False = undefined
itemIndex x f = f + x * nFeatures

-- retrieve the index of a latent user feature in the parameter vector
userIndex :: User -> Int -> Int
--userIndex x f | trace ("user: " ++ (show x) ++ " " ++  (show f)) False = undefined
userIndex x f = nItems * nFeatures  + f + x * nFeatures 

{-
(?) matrices i | trace ("lookup: " ++ (show i)) False = undefined 
(?) matrices i = matrices U.! i
-}

-- | computes the latent factor vectors for matrix factorixation
model :: DataSet -> IO Matrices
model dataset = do
  let n = (nFeatures * nUsers) + (nFeatures * nItems)
  matrices <- randomVec n
  D.withVect (V.toList dataset) (runSGD matrices dataset)

randomVec :: Int -> IO Matrices
randomVec n = fmap U.fromList $ replicateM n $ randomRIO (0,1)

-- | SGD optimizer
runSGD :: Matrices -> DataSet -> D.Dataset DataPoint -> IO Matrices
runSGD matrices all d = do
  let sgdArgs = S.sgdArgsDefault { S.iterNum = nr_iter, S.gain0=rate }
  let bm = B.model all
  S.sgd sgdArgs (notify bm all) (grad bm) d matrices 

-- notification run by the SGD function 
notify :: BiasModel -> DataSet -> Matrices -> Int -> IO ()
notify biasModel all matrices k = putStr ""
{-("\n" ++
                                          (show k) ++
                                          ("\t") ++
                                          (show (goal biasModel matrices all)))-}

-- cost function returning the squared sum of the errors
goal :: BiasModel -> Matrices -> DataSet -> Error
goal biasModel matrices all = V.sum (V.map (\x -> err2 x) all)
  where err2 (u, i, r) = (r - mu - (sum [el i u f | f <- fRange]))^2
        el i u f = matrices U.! (itemIndex i f) * (matrices U.! ((userIndex u f)))
        --bias u i = B.predict mu urm ium uirm u i
        (mu,_,_,_) = biasModel

-- gradient descent step
grad :: BiasModel -> Matrices -> DataPoint -> S.Grad
--grad biasModel matrices dataPoint | trace ("grad: " ++ (show dataPoint)) False = undefined
grad biasModel matrices dataPoint = S.fromList gradient
  where gradient = (gradP biasModel matrices dataPoint) ++ (gradQ biasModel matrices dataPoint)           

-- computes the gradient step for the item component
gradP :: BiasModel -> Matrices -> DataPoint -> [(Int, Double)]  
gradP biasModel matrices dataPoint = let
  (user,item,rating) = dataPoint
  conf = confidence rating
  err = errorAt biasModel matrices dataPoint
  in do
    f <- fRange
    let update = conf * err * (matrices U.! (userIndex user f)) - lambda * (matrices U.! (itemIndex item f))
    return (itemIndex item f, update)

-- computes the gradient step for the user component
gradQ :: BiasModel -> Matrices -> DataPoint -> [(Int, Double)]
gradQ biasModel matrices dataPoint = let
  (user,item,rating) = dataPoint
  conf = confidence rating
  err = errorAt biasModel matrices dataPoint
  in do
    f <- fRange
    let update = conf * err * (matrices U.! (itemIndex item f)) - lambda * (matrices U.! (userIndex user f))
    return (userIndex user f, update)

-- calculates error for user, item, rating with a given latent factor vector
errorAt :: BiasModel -> Matrices -> DataPoint -> Error 
errorAt biasModel matrices (user,item,rating) = preference rating - bias - prediction 
  where prediction = (sum [(matrices U.! (itemIndex item f)) * (matrices U.! (userIndex user f))| f <- fRange])
        bias = 0.0 --B.predict biasModel user item


-- see Hu et. al. "Collaborative Filtering for Implicit Feedback Datasets", section 4     
confidence :: Rating -> Double
confidence rating = 1 + alpha * rating

preference :: Rating -> Double
preference rating | rating > 1.0 = 1.0
                  | otherwise = 0.0

{-
-- computes the gradient step for the item component
gradP :: BiasModel -> Matrices -> DataPoint -> [(Int, Double)]  
gradP biasModel matrices dataPoint = do
  f <- fRange
  let (user,item,rating) = dataPoint
      err = errorAt biasModel matrices dataPoint
      update = err * (matrices U.! (userIndex user f)) - lambda * (matrices U.! (itemIndex item f))
  return (itemIndex item f, update)

-- computes the gradient step for the user component
gradQ :: BiasModel -> Matrices -> DataPoint -> [(Int, Double)]
gradQ biasModel matrices dataPoint = do
  f <- fRange
  let (user,item,rating) = dataPoint
      err = errorAt biasModel matrices dataPoint
      update = err * (matrices U.! (itemIndex item f)) - lambda * (matrices U.! (userIndex user f))
  return (userIndex user f, update)

-- calculates error for user, item, rating with a given latent factor vector
errorAt :: BiasModel -> Matrices -> DataPoint -> Error 
errorAt biasModel matrices (user,item,rating) = rating - bias - innerProd
  where innerProd = (sum [(matrices U.! (itemIndex item f)) * (matrices U.! (userIndex user f))| f <- fRange])
        bias = B.predict biasModel user item
        --(mu,urm,ium,uirm) = biasModel
-}
