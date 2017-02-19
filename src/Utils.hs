module Utils where

import qualified Data.MultiMap as MM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V


type Item = Int -- Item id
type User = Int  -- User id
type Error = Double
type Rating = Double


type ItemRatingMap = M.Map Item Rating
type UserRatings = (User, ItemRatingMap)
type UserItemRatingMap = M.Map User ItemRatingMap


type DataPoint = (User,Item,Rating)
type DataSet = V.Vector DataPoint
type BiasModel = (Rating,              --avg rating,
                  MM.MultiMap User Rating,  --keys are users. values are ratings of those users
                  MM.MultiMap Item User, --keys are items. values are users who rated that item
                  UserItemRatingMap)   --map contains all ratings to user item keys

-- collection of a user's ratings


itemUserMap :: DataSet -> MM.MultiMap Item User
itemUserMap v = V.foldl (\acc (u,i,r) -> MM.insert i u acc) MM.empty v

-- | creates mapping for user/item to rating
userItemRatingMap :: DataSet -> UserItemRatingMap
userItemRatingMap v = V.foldl insertUser M.empty v
            where insertUser acc (u, i, r) = M.insertWith (insertItem i r) u (M.singleton i r) acc
                  insertItem i r _ old = M.insert i r old

userItemRatingMap' :: [UserRatings] -> UserItemRatingMap
userItemRatingMap' = M.fromList


