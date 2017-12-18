module Hlt.Utils where

import Hlt.Constants
import Hlt.GameMap
import Hlt.Entity
import Data.List


isEnemyShip :: Ship -> GameMap -> Bool
isEnemyShip s gMap = s `elem` (listMyShips gMap)

nearbyShips :: Ship -> [Ship] -> Float -> [Ship]
nearbyShips s ships radius = filter close ships
    where close = (> radius) . (distance s)

numberAdvantageInArea :: GameMap -> Ship -> Float -> Int
numberAdvantageInArea map center radius = myNum - eNum 
    where myNum = length $ nearbyShips center mShips radius
          eNum = length $ nearbyShips center eShips radius
          mShips = listAllShips map
          eShips = listEnemyShips map

-- probably shouldn't use this one really
getNotCrashSpd :: Float -> Float -> Float
getNotCrashSpd distance spd 
    | distance > 0 = minimum [distance,spd]
    | otherwise = 0

getClosestFromList :: Entity a => Entity b => a -> [b] -> b
getClosestFromList e1 list = minimumBy comp list
    where comp = \a b -> compare (distance e1 a) (distance e1 b)

allFreePlanet :: GameMap -> [Planet]
allFreePlanet m = filter (not . isOwned) $ listAllPlanets m

-- as in, in a bunch of entities
longestDistanceInEntities :: Entity a => [a] -> Float
longestDistanceInEntities es = maximum allDist
    where allDist = [distance a b | a <- es, b <- es]

-- distance to closest is farther than the above function value
farAwayFromEntities :: Entity a => a -> [a] -> Bool
farAwayFromEntities e le = distToList > longestDistInList
    where distToList = distance e $ getClosestFromList e le
          longestDistInList = longestDistanceInEntities le

closestEnemyShip :: Entity a => a -> GameMap -> Ship
closestEnemyShip e m = getClosestFromList e $ listEnemyShips m 

closestEnemyPlanet :: Entity a => a -> GameMap -> Planet
closestEnemyPlanet e m = getClosestFromList e $ listEnemyPlanet m

getTotalProduction :: [Planet] -> Int
getTotalProduction ps = sum $ map production ps


listDockablePlanet :: GameMap -> [Planet]
listDockablePlanet m = allFreePlanet m ++ myUnderPs
    where myUnderPs 
             | length (listMyPlanet m ) > 0 = filter (not . isFull) $ listMyPlanet m
             | otherwise = []


