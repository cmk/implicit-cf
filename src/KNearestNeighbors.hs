module KNearestNeighbors where -- (model, recommend) where 

import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe

import Utils

{-
doWork :: [Record] -> IO ()
doWork csv = let
  users = map avg $ M.elems $ M.fromListWith combine $ map parseToTuple $ tail $ init csv
  user = avg $ (,) (-1) $ M.fromList [(8,2),(48,1)]
  closests = take 30 $ UB.sortNeighbors UB.cosine user users
  recs = UB.recommend UB.cosine user users
  r = UB.ratings $ UB.findUser (snd $ head closests) users :: ItemRatingMap
  in do
    --print users
    print closests
    print recs
    print user
    print r
-}

avg :: UserRatings -> UserRatings
avg (user,ratings) = let
  sum = M.foldl' (+) 0.0 ratings
  ratings' = M.map (\r -> r/sum) ratings
  in (user, ratings')



-- | Return just the item ratings for the given sample
ratings :: UserRatings -> ItemRatingMap
ratings = snd



-- | Find the user in the list of samples
findUser :: [UserRatings] -> User -> Maybe UserRatings
findUser users u = let
  isUser (k, _) = k == u
  in find isUser users

-- | Apply the given distance function across all eligable ratings
-- | Eligible ratings are the intersection of l and r
distanceWith :: ([Rating] -> [Rating] -> Rating) -> ItemRatingMap -> ItemRatingMap -> Rating
distanceWith f l r = f lItems rItems
  where lIsect = M.intersection l r
        rIsect = M.intersection r l
        lItems = M.elems lIsect
        rItems = M.elems rIsect


jaccard :: UserRatings -> UserRatings -> Double
jaccard l r = let
  lr = ratings l
  rr = ratings r
  dist l r = let
    num = fromIntegral $ M.size $ M.intersection l r :: Double
    denom = fromIntegral $ M.size $ M.union l r :: Double
    in num / denom  
  in dist lr rr 

        
-- | compute the Jaccard distances for the given samples from the user, then sort by closest
sortNeighbors :: [UserRatings] -> UserRatings -> [(Double,UserRatings)]
sortNeighbors s u = let
  getDist sample = (jaccard u sample, sample)
  sorted = reverse $ sort $ map getDist s
  in sorted
       

-- | Return the head from sortNeighbors
kNearestNeighbors :: Int -> [UserRatings] -> UserRatings -> [(Double,UserRatings)]
kNearestNeighbors k users u = take k $ sortNeighbors users u

getRatings :: Int -> [UserRatings] -> UserRatings -> [(Item,Rating)]
getRatings k users u = let
  neighbors = kNearestNeighbors k users u
  convert (dist,user) = map (\(item,rating) -> (item,rating * dist)) $ M.toList $ snd user  --weight ratings by distance from user
  ratings = neighbors >>= convert
  compare (a,_) (a',_) = a == a'
  in nubBy compare ratings

-- | Recommend items from the 10 nearest neighbors in the samples list for the given user,
-- | sorted by highest scored item first

recommend :: [UserRatings] -> [User] -> [[Item]]
recommend users testData = let
  makeRec u = let
    defaultItems = [1,2,0,5,6,4,8,13,14,11] --most bought items
    defaultUser = (-1, M.fromList $ map (flip (,) $ 1) defaultItems)
    u' = fromMaybe defaultUser $ findUser users u-- if user is not in the training dataset then default to recommendations for default user.
    userRecs = getRatings 20 users u'
    highestFirst (_,s1) (_,s2)
            | s1 > s2 = LT
            | s1 < s2 = GT
            | otherwise = EQ
    out = map fst $ sortBy highestFirst $ userRecs
    in take 10 $ nub $ out ++ defaultItems
  in map makeRec testData
              


