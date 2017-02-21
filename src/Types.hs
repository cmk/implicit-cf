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
type BiasModel = (Rating,
                  MM.MultiMap User Rating,
                  MM.MultiMap Item User,
                  UserItemRatingMap)
