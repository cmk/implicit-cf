module MatrixFactorization  where --(Env, model, recommend, predict)

import Control.Applicative 
import Control.Monad (replicateM)
import Control.Monad.Reader
import Data.List (foldl',sort)
import Numeric (showFFloat)
import System.Random (randomRIO)
import Text.Printf

import qualified Data.Map as M
import qualified Data.Matrix as DM
import qualified Data.MultiMap as MM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Numeric.SGD as S
import qualified Numeric.SGD.Dataset as D

import qualified Bias as B
import Types

type Error = Double
type Matrices = U.Vector Double -- factor matrices P, Q stored in row-major order


{- notes / TODO

this is a 
see Hu et. al. "Collaborative Filtering for Implicit Feedback Datasets"


user Reader to encode config stuff like Matrices, BiasModel etc
fix gradP, gradQ repetition with ap or map2 or somesuch
-}

{-
rate = 0.2 -- initial learning rate
alpha = 15.0 -- confidence scaling factor, see Hu et al section 4
lambda = 0.1 -- Tikhonov regularization paramater

nIters = 80
nFeatures = 10
nUsers = 12 --24381
nItems = 11 --300
fRange = [0..nFeatures-1]

-}
--
-- with Reader, alternate
--getDogR' :: Person -> Dog
--getDogR' = liftA2 Dog dogName address

data Env = Env {
  rate :: Double ,
  alpha :: Double ,
  lambda :: Double ,
  nIters :: Int ,
  nUsers :: Int ,
  nItems :: Int ,
  nFeatures :: Int,
  fRange :: Int
} deriving (Eq, Show)


data Env = Env { nIt :: Int , nFeat :: Int}
  
recommend :: Matrices -> [User] -> [[Item]]
recommend matrices users = let
  recs = getR matrices -- rows are items for a particular user
  makeRec user = let
    row = if user < nUsers then user+1 else 1 -- if user is not in the training dataset then default to recommendations for first user.
    userRecs = zip (V.toList $ DM.getRow row recs) [1..]
    in map (\(_,x) -> x-1) $ take 10 $ reverse $ sort $ userRecs
  in map makeRec users
  
-- retrieve the index of an latent item feature in the parameter vector
itemIndex :: Item -> Int -> Int
itemIndex x f = f + x * nFeatures

-- retrieve the index of a latent user feature in the parameter vector
userIndex :: User -> Int -> Int
userIndex x f = nItems * nFeatures  + f + x * nFeatures 

--userIndex' :: User -> Int -> Reader Env Int
userIndex' x f = let
  out nIte nFeate = nIte * nFeate  + f + x * nFeate
  in liftA2 out nIt nFeat

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
  let sgdArgs = S.sgdArgsDefault { S.iterNum = nIters, S.gain0=rate }
  let bm = B.model all
  S.sgd sgdArgs (notify bm all) (grad bm) d matrices 

-- notification run by the SGD function 
notify :: BiasModel -> DataSet -> Matrices -> Int -> IO ()
notify biasModel dataSet matrices k = putStr ("\n" ++
                                          (show k) ++
                                          ("\t") ++
                                          (show (objective biasModel matrices dataSet)))


objective :: BiasModel -> Matrices -> DataSet -> Error
objective biasModel matrices dataSet = let
  err2 x = (errorAt biasModel matrices x)**2
  in V.sum (V.map (\x -> err2 x) dataSet)
  
-- gradient descent step
grad :: BiasModel -> Matrices -> DataPoint -> S.Grad
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
        bias = B.predict biasModel user item

-- see Hu et. al. "Collaborative Filtering for Implicit Feedback Datasets", section 4     
confidence :: Rating -> Double
confidence rating = 1 + alpha * rating

preference :: Rating -> Double
preference rating | rating > 0.0 = 1.0
                  | otherwise = 0.0


-- | computes a prediction for the rating of user u on item i 
predict :: BiasModel -> Matrices -> User -> Item -> Rating
predict biasModel matrices user item = let
  bias = B.predict biasModel user item
  prediction = sum [(matrices U.! (itemIndex item f)) * (matrices U.! (userIndex user f))| f <- fRange]
  in bias + prediction


-- retrieve factor matrices P,Q and full matrix R = Q*P^t, for debugging only
getP matrices = let
  l = take (nItems * nFeatures) $ U.toList matrices
  in DM.fromList nItems nFeatures l

getQ matrices = let
  l = drop (nItems * nFeatures) $ U.toList matrices
  in DM.fromList nUsers nFeatures l
  
getR matrices = let
  p = getP matrices
  q = getQ matrices
  in DM.multStd q $ DM.transpose p -- rows are users, cols are items

showMat :: DM.Matrix Double -> String
showMat m = show $ fmap (\i -> showFFloat (Just 2) i "") m
