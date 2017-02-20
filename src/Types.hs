module Types where

import qualified Data.Map as M
import qualified Data.MultiMap as MM
import qualified Data.Vector as V

type Item = Int -- Item id
type User = Int  -- User id
type Rating = Double -- Number of purchases, normalized in KNN

type ItemRatingMap = M.Map Item Rating -- A users item ratings
type UserRatings = (User, ItemRatingMap)
type UserItemRatingMap = M.Map User ItemRatingMap

type DataPoint = (User,Item,Rating)
type DataSet = V.Vector DataPoint
type BiasModel = (Rating,              --avg rating,
                  MM.MultiMap User Rating,  --keys are users. values are ratings of those users
                  MM.MultiMap Item User, --keys are items. values are users who rated that item
                  UserItemRatingMap)   --map contains all ratings to user item keys




