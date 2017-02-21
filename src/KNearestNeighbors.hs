module KNearestNeighbors where 

import Data.List
import Data.Maybe

import qualified Data.Map.Strict as M

import Types


-- | Find the user in the list of samples
findUser :: [UserRatings] -> User -> Maybe UserRatings
findUser users u = let
  isUser (k, _) = k == u
  in find isUser users

-- | Get the Jaccard distance between 2 users
jaccard :: UserRatings -> UserRatings -> Double
jaccard l r = let
  lr = snd l
  rr = snd r
  dist l r = let
    num = fromIntegral $ M.size $ M.intersection l r :: Double
    denom = fromIntegral $ M.size $ M.union l r :: Double
    in num / denom  
  in dist lr rr 
        
-- | Compute the Jaccard distances from the user, then sort by closest
sortNeighbors :: [UserRatings] -> UserRatings -> [(Double,UserRatings)]
sortNeighbors s u = let
  getDist sample = (jaccard u sample, sample)
  sorted = reverse $ sort $ map getDist s
  in sorted
       
-- | Return the top k users from sortNeighbors
kNearestNeighbors :: Int -> [UserRatings] -> UserRatings -> [(Double,UserRatings)]
kNearestNeighbors k users u = take k $ sortNeighbors users u

-- | Recommend unique items from the k nearest neighbors in trainData for the given user
-- weighted by item ratings and distance from user
getRatings :: Int -> [UserRatings] -> UserRatings -> [(Item,Rating)]
getRatings k users u = let
  neighbors = kNearestNeighbors k users u
  convert dist user = let
    weight item rating = (item,rating * dist)
    in map (uncurry weight) $ M.toList $ snd user  
  ratings = neighbors >>= uncurry convert
  compare (a,_) (a',_) = a == a'
  in nubBy compare ratings

-- | Return the top 10 recommendations for each user in a list
recommend :: [UserRatings] -> [User] -> [[Item]]
recommend trainData testData = let
  makeRec u = let
    users = map avg trainData
    --most purchased items in this dataset
    defaultItems = [1,2,0,5,6,4,8,13,14,11] 
    defaultUser = (-1, M.fromList $ map (flip (,) $ 1) defaultItems)
    -- if user is not in the training dataset then use a default
    u' = fromMaybe defaultUser $ findUser users u
    -- k=20 seems to work well for this dataset
    userRecs = getRatings 20 users u'
    highestFirst (_,s1) (_,s2)
            | s1 > s2 = LT
            | s1 < s2 = GT
            | otherwise = EQ
    out = map fst $ sortBy highestFirst $ userRecs
    --if there are fewer than 10 recs pad list with popular items
    in take 10 $ nub $ out ++ defaultItems
  in map makeRec testData
              
-- | Normalize user so that purchases sum to 1.0
avg :: UserRatings -> UserRatings
avg (user,ratings) = let
  sum = M.foldl' (+) 0.0 ratings
  ratings' = M.map (\r -> r/sum) ratings
  in (user, ratings')

