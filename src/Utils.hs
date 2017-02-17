module Utils where

import qualified Data.MultiMap as MM
import qualified Data.Map as M
import qualified Data.Vector as V

type Item = Int -- Item id
type User = Int  -- User id
type Error = Double
type Rating = Double
type UserRatingMap = MM.MultiMap User Rating
type ItemUserMap = MM.MultiMap Item User
type ItemRatingMap = M.Map Item Rating
type UserItemRatingMap = M.Map User ItemRatingMap
type DataPoint = (User,Item,Rating)
type DataSet = V.Vector DataPoint
type BiasModel = (Rating,              --avg rating,
                  UserRatingMap,       --keys are users. values are ratings of those users
                  ItemUserMap,         --keys are items. values are users who rated that item
                  UserItemRatingMap)   --map contains all ratings to user item keys


itemUserMap :: DataSet -> ItemUserMap
itemUserMap v = V.foldl (\acc (u,i,r) -> MM.insert i u acc) MM.empty v

-- | creates mapping for user/item to rating
userItemRatingMap :: DataSet -> UserItemRatingMap
userItemRatingMap v = V.foldl insertUser M.empty v
            where insertUser acc (u, i, r) = M.insertWith (insertItem i r) u (M.singleton i r) acc
                  insertItem i r _ old = M.insert i r old

