module MatrixFactorization (model, predict) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U 
import qualified Data.MultiMap as MM
import qualified Data.Map as M

import qualified Numeric.SGD as S
import qualified Numeric.SGD.Dataset as D
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))
import Data.List (foldl')
import qualified Bias as B

import Utils

type Matrices = U.Vector Double -- factor matrices P, Q^t stored in row-major order


{- notes / TODO

user Reader to encode config stuff like Matrices, BiasModel etc

-}


rate = 0.01 -- initial learning rate
alpha = 40 -- confidence scaling factor, see Hu et al section 4
lambda = 0.01 -- Tikhonov regularization paramater

nr_iter = 2
nFeatures = 10
nUsers = 943
nItems = 1682
fRange = [0..nFeatures-1]
datasetSize = 80000

-- | computes a prediction for the rating of user u on item i 
predict :: BiasModel -> Matrices -> User -> Item -> Rating
predict biasModel matrices user item = let
  bias = B.predict biasModel user item
  prediction = sum [(matrices U.! (itemIndex item f)) * (matrices U.! (userIndex user f))| f <- fRange]
  in bias + prediction

-- retrieve the index of an latent item feature in the parameter vector
itemIndex :: Item -> Int -> Int 
itemIndex x f = ((x-1) * nFeatures) + f

-- retrieve the index of a latent user feature in the parameter vector
userIndex :: User -> Int -> Int 
userIndex x f = nItems + ((x-1) * nFeatures) + f



-- | computes the latent factor vectors for matrix factorixation
model :: DataSet -> IO Matrices
model dataset = do 
  let n = (nFeatures * nUsers) + (nFeatures * nItems)
  let matrices = U.replicate n (0.1::Double)
  D.withVect (V.toList dataset) (runSGD matrices dataset)


-- | SGD optimizer
runSGD :: Matrices -> DataSet -> D.Dataset DataPoint -> IO Matrices
runSGD matrices all d = do
  let sgdArgs = S.sgdArgsDefault { S.iterNum = nr_iter, S.gain0=rate }
  let bm = B.model all
  S.sgd sgdArgs (notify bm all) (grad bm) d matrices 

-- notification run by the SGD function 
notify :: BiasModel -> DataSet -> Matrices -> Int -> IO ()
notify biasModel all matrices k = putStr ("\n" ++
                                          (show k) ++
                                          ("\t") ++
                                          (show (goal biasModel matrices all)))

-- cost function returning the squared sum of the errors
goal :: BiasModel -> Matrices -> DataSet -> Error
goal biasModel matrices all = V.sum (V.map (\x -> err2 x) all)
  where err2 (u, i, r) = (r - mu - (sum [el i u f | f <- fRange]))^2
        el i u f = matrices U.! (itemIndex i f) * (matrices U.! ((userIndex u f)))
        --bias u i = B.predict mu urm ium uirm u i
        (mu,_,_,_) = biasModel

-- gradient descent step
grad :: BiasModel -> Matrices -> DataPoint -> S.Grad
grad biasModel matrices dataPoint = S.fromList gradient
  where gradient = (gradP biasModel matrices dataPoint) ++ (gradQ biasModel matrices dataPoint)           

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
        
