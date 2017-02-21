module MatrixFactorization where --(model, recommend, predict) 

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
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

type Matrices = U.Vector Double -- factor matrices P, Q stored in row-major order



rate = 0.1 -- Initial learning rate. 
alpha = 2.0 -- Confidence scaling factor, see Hu et al section 4. Note that SGD requires a much smaller alpha than ALS.
lambda = 0.4 -- Tikhonov regularization paramater

nr_iter = 30
nFeatures = 10
nUsers = 28606 -- User numbers are mapped directly to indices for now. Actual number of users is 24429.
nItems = 300
fRange = [0..nFeatures-1]


-- | Generates 10 recommendations for each user in the list
recommend :: Matrices -> [User] -> [[Item]]
recommend matrices users = let
  recs = getR matrices --rows are items for a particular user
  makeRec user = let
    -- if user is not in the training dataset then default to recommendations for first user.
    row = if user < nUsers then user+1 else 1 
    userRecs = zip (V.toList $ DM.getRow row recs) [1..]
    in map (\(_,x) -> x-1) $ take 10 $ reverse $ sort $ userRecs
  in map makeRec users
  

 
-- | Computes the latent factor vectors for matrix factorixation
model :: [UserRatings] -> IO Matrices
model trainData = do
  let input = dataSet trainData
      n = (nFeatures * nUsers) + (nFeatures * nItems)
  -- initialize to random floats in [0,1]
  matrices <- fmap U.fromList $ replicateM n $ randomRIO (0.05,0.15)
  D.withVect (V.toList input) (runSGD matrices input)

-- | 
dataSet :: [UserRatings] -> DataSet
dataSet users = let
  tuple a (b,c) = (a,b,c)
  dataPoints (user,ratings) = map (tuple user) $ M.toList ratings
  in V.fromList $ users >>= dataPoints


-- | SGD optimizer
runSGD :: Matrices -> DataSet -> D.Dataset DataPoint -> IO Matrices
runSGD matrices all d = do
  let sgdArgs = S.sgdArgsDefault { S.iterNum = nr_iter, S.gain0=rate }
  let bm = B.model all
  S.sgd sgdArgs (notify bm all) (grad bm) d matrices 

-- | Notification run by the SGD function 
notify :: BiasModel -> DataSet -> Matrices -> Int -> IO ()
notify biasModel dataSet matrices k = putStr ("\n" ++
                                          (show k) ++
                                          ("\t") ++
                                          (show (objective biasModel matrices dataSet)))

-- | Overall objective function that we are trying to minimize
objective :: BiasModel -> Matrices -> DataSet -> Double
objective biasModel matrices dataSet = let
  err2 x = (errorAt x biasModel matrices)**2
  in V.sum (V.map (\x -> err2 x) dataSet)


-- | Gradient descent step
grad :: BiasModel -> Matrices -> DataPoint -> S.Grad
grad biasModel matrices dataPoint = let
  gradient = liftA2 (++)
                    (gradP dataPoint biasModel)
                    (gradQ dataPoint biasModel)
                    matrices
  in S.fromList gradient         

-- | Computes the gradient step for the item component
gradP :: DataPoint -> BiasModel -> Matrices -> [(Int, Double)]  
gradP dataPoint biasModel matrices  = let
  (user,item,rating) = dataPoint
  conf = confidence rating
  err = errorAt dataPoint biasModel matrices
  --partial derivative of Eq 3 in Hu et. al. wrt y
  partial p q = conf * err * q - lambda * p
  in do
    f <- fRange
    let idx = f + item * nFeatures 
        update = liftA2 partial (itemIdx item f) (userIdx user f) matrices
    return (idx, update)


-- | Computes the gradient step for the user component
gradQ :: DataPoint -> BiasModel -> Matrices -> [(Int, Double)]
gradQ dataPoint biasModel matrices = let
  (user,item,rating) = dataPoint
  conf = confidence rating
  err = errorAt dataPoint biasModel matrices
  --partial derivative of Eq 3 in Hu et. al. wrt x
  partial p q = conf * err * p - lambda * q
  in do
    f <- fRange
    let offset = nItems * nFeatures
        idx = offset + f + item * nFeatures 
        update = liftA2 partial (itemIdx item f) (userIdx user f) matrices
    return (idx, update)


-- | Computes the error for user, item, rating with a given latent factor vector
errorAt :: DataPoint -> BiasModel -> Matrices -> Double
errorAt dataPoint biasModel matrices = let
  (user,item,rating) = dataPoint
  bias = B.predict biasModel user item
  dotProd f = return $ liftA2 (*) (itemIdx item f) (userIdx user f) matrices
  prediction = sum $ fRange >>= dotProd
  in preference rating - bias - prediction 

   
-- | Computes a prediction for the rating of user u on item i 
predict :: BiasModel -> Matrices -> User -> Item -> Double
predict biasModel matrices user item = let
  bias = B.predict biasModel user item
  dotProd f = return $ liftA2 (*) (itemIdx item f) (userIdx user f) matrices
  prediction = sum $ fRange >>= dotProd
  in bias + prediction

  
-- | Retrieve the index of an latent item feature in the parameter vector
itemIdx :: Int -> Int -> Matrices -> Double
itemIdx x f m = let
  idx = f + x * nFeatures
  in m U.! idx

-- | Retrieve the index of a latent user feature in the parameter vector
userIdx :: Int -> Int -> Matrices -> Double
userIdx x f m = let
  offset = nItems * nFeatures
  idx = offset + f + x * nFeatures 
  in m U.! idx


-- | Confidence rating in our prediction that a user rates an item highly.
-- See Hu et. al. "Collaborative Filtering for Implicit Feedback Datasets", section 4     
confidence :: Rating -> Double
confidence rating = let
  eps = 10.0
  in 1 + alpha * rating
  --1 + alpha * logBase 2 (rating / eps)

preference :: Rating -> Double
preference rating | rating > 0.0 = 1.0
                  | otherwise = 0.0


-- | Retrieve factor matrices P,Q and full matrix R = Q*P^t, for debugging only
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
