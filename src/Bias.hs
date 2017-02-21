module Bias (model, predict) where

import qualified Data.MultiMap as MM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Types


-- | Create dictionaries for unpersonalized recommendation
model :: DataSet -> BiasModel
model vec =  (mu, urm, ium, uirmap)
  where mu = (V.sum $ V.map (\(u,i,r)->r) vec) / (fromIntegral $ V.length vec)
        urm = V.foldl (\acc (u,_,r) -> MM.insert u r acc) MM.empty vec
        ium = V.foldl (\acc (u,i,r) -> MM.insert i u acc) MM.empty vec
        uirmap = V.foldl insertUser M.empty vec
        insertUser acc (u, i, r) = M.insertWith (insertItem i r) u (M.singleton i r) acc
        insertItem i r _ old = M.insert i r old
        
-- | Predict the rating for user us and item it, zero'ed out for the time being
predict :: BiasModel -> User -> Item -> Double
predict biasModel us it = 0.0 --mu + (deviation mu urm us) + bi
  where (mu,urm,ium,uirm) = biasModel
        dev = map (\x -> rating x it - (deviation mu urm x) - mu) users
        s = sum $ dev 
        n = fromIntegral $ length users
        rating u i = rui u i (irm u)  
        userlength = fromIntegral $ length users
        bi = s / n
        rui u i m = M.findWithDefault 0 i m
        users = MM.lookup it ium 
        irm u = M.findWithDefault M.empty u uirm

-- | Predict user deviation from global average
deviation :: Double -> MM.MultiMap User Rating -> User -> Double
deviation mu urm u = bu
  where ratingsOfUser = MM.lookup u urm
        dev = sum (map (\x -> x - mu) $ ratingsOfUser)
        bu = dev / (fromIntegral $ length ratingsOfUser)
