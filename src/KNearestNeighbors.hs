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
findUser :: User -> [UserRatings] -> Maybe UserRatings
findUser user s = let
  isUser (k, _) = k == user
  in find isUser s

-- | Apply the given distance function across all eligable ratings
-- | Eligible ratings are the intersection of l and r
distanceWith :: ([Rating] -> [Rating] -> Rating) -> ItemRatingMap -> ItemRatingMap -> Rating
distanceWith f l r = f lItems rItems
  where lIsect = M.intersection l r
        rIsect = M.intersection r l
        lItems = M.elems lIsect
        rItems = M.elems rIsect

jaccard :: ItemRatingMap -> ItemRatingMap -> Rating
jaccard l r = let
  num = fromIntegral $ M.size $ M.intersection l r :: Double
  denom = fromIntegral $ M.size $ M.union l r :: Double
  in num / denom
  
computeDistance :: UserRatings -> UserRatings -> Rating
computeDistance l r = jaccard lr rr 
  where lr = ratings l
        rr = ratings r

-- | Compute the distances for the given samples from the user, using the function supplied, then sort by closest
sortNeighbors :: UserRatings -> [UserRatings] -> [(Rating, User)]
sortNeighbors u s = reverse $ sort $ map dist s
  where iden (a, m) = a
        dist sample = (computeDistance u sample, iden sample)

-- | Return the head from sortNeighbors
closestNeighbor :: UserRatings -> [UserRatings] -> (Rating, User)
closestNeighbor u s = head $ sortNeighbors u s

closestRatings :: UserRatings -> [UserRatings] -> ItemRatingMap
closestRatings u s = nrRatings
  where neighbor = snd $ closestNeighbor u s
        nrRatings = ratings $ fromJust $ findUser neighbor s

-- | Recommend items from the nearest neighbor in the samples list for the given user,
-- | sorted by highest scored item first
recommend' :: UserRatings -> [UserRatings] -> [(Item,Rating)]
recommend' u s = sortBy highestFirst recommendations
  where uRatings  = ratings u
        clRatings = closestRatings u s
        recommendations = M.toList clRatings 
        highestFirst (_,s1) (_,s2)
                    | s1 > s2 = LT
                    | s1 < s2 = GT
                    | otherwise = EQ

recommend :: [UserRatings] -> [User] -> [[Item]]
recommend model users = let
  makeRec user = let
    defaultUser = fromJust $ findUser 0 model
    user' = fromMaybe defaultUser $ findUser user model -- if user is not in the training dataset then default to recommendations for first user.
    clRatings = closestRatings user' model
    userRecs = M.toList clRatings
    highestFirst (_,s1) (_,s2)
            | s1 > s2 = LT
            | s1 < s2 = GT
            | otherwise = EQ
    in map fst $ take 10 $ sortBy highestFirst $ userRecs
  in map makeRec users
              


