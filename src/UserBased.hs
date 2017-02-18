module UserBased where

import qualified Data.Map.Strict as M
import Data.List

import Utils

{-
doWork :: [Record] -> IO ()
doWork csv = let
  users = map avg $ M.elems $ M.fromListWith combine $ map parseToTuple $ tail $ init csv
  user = avg $ (,) (-1) $ M.fromList [(8,2),(48,1)]
  closests = take 30 $ UB.sortNeighbours UB.cosine user users
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
findUser :: User -> [UserRatings] -> UserRatings
findUser user s = head $ filter isUser s
  where isUser (k, _) = k == user

-- | The minkowski distance function, where r is the exponent
minkowski :: Rating -> Rating -> Int -> Rating
minkowski a b r = dif ** ex
  where dif = (abs (a - b)) :: Double
        ex  = (fromIntegral r)

-- | The manhattan distance function
manhattan :: [Rating] -> [Rating] -> Rating
manhattan [] _ = 0
manhattan _ [] = 0
manhattan l r = foldl mink 0 $ zip l r
  where mink u (a,b) = u + minkowski a b 1

-- | The euclidean distance function
euclidean :: [Rating] -> [Rating] -> Rating
euclidean l r = sqrt $ foldl mink 0 $ zip l r
  where mink u (a,b) = u + minkowski a b 2

-- | The cosine distance function
cosine :: [Rating] -> [Rating] -> Rating
cosine [] _ = 0
cosine _ [] = 0
cosine l r = let
  n = length l
  zero = replicate n 0.0
  lnorm = euclidean l zero
  rnorm = euclidean r zero
  denom = lnorm * rnorm
  num = sum $ zipWith (*) l r
  in num / denom

pearson  :: [Rating] -> [Rating] -> Rating
pearson [] _ = 0
pearson _ [] = 0
pearson l r = let
  n = (fromIntegral $ length l) :: Double
  lavg = sum l / n
  ravg = sum r / n
  l' = map (lavg-) l
  r' = map (ravg-) r
  in cosine l' r'
           
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
  
computeDistance :: ([Rating] -> [Rating] -> Rating) -> UserRatings -> UserRatings -> Rating
computeDistance f l r = jaccard lr rr --distanceWith f lr rr
  where lr = ratings l
        rr = ratings r

-- | Compute the distances for the given samples from the user, using the function supplied, then sort by closest
sortNeighbours :: ([Rating] -> [Rating] -> Rating) -> UserRatings -> [UserRatings] -> [(Rating, User)]
sortNeighbours f u s = reverse $ sort $ map dist s
  where iden (a, m) = a
        dist sample = (computeDistance f u sample, iden sample)

-- | Return the head from sortNeighbours
closestNeighbour :: ([Rating] -> [Rating] -> Rating) -> UserRatings -> [UserRatings] -> (Rating, User)
closestNeighbour f u s = head $ sortNeighbours f u s

closestRatings :: ([Rating] -> [Rating] -> Rating) -> UserRatings -> [UserRatings] -> ItemRatingMap
closestRatings f u s = nrRatings
  where neighbour = snd $ closestNeighbour f u s
        nrRatings = ratings $ findUser neighbour s

-- | Recommend items from the nearest neighbour in the samples list for the given user,
-- | sorted by highest scored item first
recommend :: ([Rating] -> [Rating] -> Rating) -> UserRatings -> [UserRatings] -> [(User, Rating)]
recommend f u s = sortBy highestFirst recommendations
  where uRatings  = ratings u
        clRatings = closestRatings f u s
        recommendations = M.toList clRatings 
        highestFirst (_,s1) (_,s2)
                    | s1 > s2 = LT
                    | s1 < s2 = GT
                    | otherwise = EQ
