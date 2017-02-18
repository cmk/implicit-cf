module UserBased where

import qualified Data.Map as M
import Data.List

import Utils

  
-- | Return just the item ratings for the given sample
ratings :: UserRatings -> UserRatingMap
ratings (UserRatings a m) = m



-- | Find the user in the list of samples
findUser :: User -> [UserRatings] -> UserRatings
findUser user s = head $ filter isUser s
  where isUser (UserRatings k _) = k == user

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
distanceWith :: ([Rating] -> [Rating] -> Rating) -> UserRatingMap -> UserRatingMap -> Rating
distanceWith f l r = f lItems rItems
  where lIsect = M.intersection l r
        rIsect = M.intersection r l
        lItems = M.elems lIsect
        rItems = M.elems rIsect

jaccard :: UserRatingMap -> UserRatingMap -> Rating
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
  where iden (UserRatings a m) = a
        dist sample = (computeDistance f u sample, iden sample)

-- | Return the head from sortNeighbours
closestNeighbour :: ([Rating] -> [Rating] -> Rating) -> UserRatings -> [UserRatings] -> (Rating, User)
closestNeighbour f u s = head $ sortNeighbours f u s

closestRatings :: ([Rating] -> [Rating] -> Rating) -> UserRatings -> [UserRatings] -> UserRatingMap
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
