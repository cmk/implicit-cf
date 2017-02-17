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

type Item = Int -- Item id
type User = Int  -- User id
type Error = Double
type Rating = Double
type UserRatingMap = MM.MultiMap User Rating
type ItemUserMap = MM.MultiMap Item User
type ItemRatingMap = M.Map Item Rating
type UserItemRatingMap = M.Map User ItemRatingMap
type DataPoint = (User,Item,Rating)
type Matrices = U.Vector Double -- fitted factor matrices P, Q^t stored in row-major order


{- notes / TODO

user Reader to encode config stuff like Matrices, BiasModel etc



-}
 
type BiasModel = (Rating,              --avg rating,
                  UserRatingMap,       --keys are users. values are ratings of those users
                  ItemUserMap,         --keys are items. values are users who rated that item
                  UserItemRatingMap)   --map contains all ratings to user item keys

alpha = 0.01 -- initial learning rate
lambda = 0.01 -- Tikhonov regularization paramater
nr_iter = 1
nFeatures = 10
nUsers = 100
nItems = 100
fRange = [0..nFeatures-1]
datasetSize = 100


-- | computes a prediction for rating of user u on item i based on the latent factor vectors
predict :: Matrices -> BiasModel -> User -> Item -> Rating
predict para biasModel user item = bias + sum [(para U.! (itemIndex item f)) * (para U.! (userIndex user f))| f <- fRange]
                      where bias = B.predict mu urm ium uirm user item
                            (mu,urm,ium,uirm) = biasModel

-- retrieve the index of an latent item feature in the parameter vector
itemIndex :: Item -> Int -> Int 
itemIndex x f = ((x-1) * nFeatures) + f

-- retrieve the index of a latent user feature in the parameter vector
userIndex :: User -> Int -> Int 
userIndex x f = nItems + ((x-1) * nFeatures) + f



-- | computes the latent factor vectors for matrix factorixation
model :: V.Vector DataPoint -> IO Matrices
model dataset = do 
    let ium = itemUserMap dataset
    let uirm = userItemRatingMap dataset
    let n = (nFeatures * nUsers) + (nFeatures * nItems)
    let matrices = U.replicate n (0.1::Double)
    D.withVect (V.toList dataset) (runSGD matrices ium uirm (V.toList dataset))

itemUserMap :: V.Vector DataPoint -> ItemUserMap
itemUserMap v = V.foldl' (\acc (u,i,r) -> MM.insert i u acc) MM.empty v

userItemRatingMap :: V.Vector DataPoint -> UserItemRatingMap
userItemRatingMap v = V.foldl' ins2 M.empty v

ins2 :: UserItemRatingMap -> DataPoint -> UserItemRatingMap
ins2 acc (u, i, r)= M.insertWith (ins3 i r) u (M.singleton i r) acc

ins3 :: Item -> Rating -> ItemRatingMap -> ItemRatingMap -> ItemRatingMap
ins3 i r _ old = M.insert i r old




-- | SGD optimizer
runSGD :: Matrices -> ItemUserMap -> UserItemRatingMap -> [DataPoint] -> D.Dataset DataPoint -> IO Matrices
runSGD matrices ium uirm all d = do
  let sgdArgs = S.sgdArgsDefault { S.iterNum = nr_iter, S.gain0=alpha }
  --let uim = userItemMap all
  --let avg = mu all
  let bm = B.model (V.fromList all)
  S.sgd sgdArgs (notify bm all) (grad bm) d matrices 

-- Notification run by the SGD function every parameters update.
notify :: BiasModel -> [DataPoint] -> Matrices -> Int -> IO ()
notify biasModel dataPoint matrices k = putStr ("\n" ++ (show k) ++ ("\t") ++ (show (goal biasModel matrices dataPoint)))

-- cost function returning the squared sum of the errors.
goal :: BiasModel -> Matrices -> [DataPoint] -> Error
goal biasModel matrices xs = sum (map (\x -> err2 x) xs)
  where err2 (u, i, r) = (r - mu - (sum [el i u f | f <- fRange]))^2
        el i u f = matrices U.! (itemIndex i f) * (matrices U.! ((userIndex u f)))
        --bias u i = B.predict mu urm ium uirm u i
        (mu,urm,ium,uirm) = biasModel

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
      update = err * (matrices U.! (userIndex user f)) - lambda * (matrices U.! (itemIndex user f))
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
        bias = B.predict mu urm ium uirm user item
        (mu,urm,ium,uirm) = biasModel
        
