module Hlt.DistributeShips where

import Hlt.Entity
import Hlt.GameMap
import Data.List
import Hlt.Utils
import Hlt.PlayersInfo

data Distribute =  Distribute { explore :: [Ship],
                                attacks :: [Ship],
                                gathers :: [Ship]} deriving (Eq,Show)


---- work on this. maybe decide depends on planet?
distributeExploreAttackGather :: GameMap -> Distribute
distributeExploreAttackGather gmap 
  | goodProduction = Distribute{attacks = fH, explore = sH, gathers = []}
  | otherwise = Distribute{explore = mySS, attacks = [], gathers = []}
  where mySS = myUndockedShips gmap
        myPs = listMyPlanet gmap
        goodProduction = 120 < (sum $ map production myPs )
        (fH, sH) = splitShipOnDistanceToPlanetAndEnemy gmap 

splitShipOnDistanceToPlanetAndEnemy :: GameMap -> ([Ship],[Ship])
splitShipOnDistanceToPlanetAndEnemy m = (atk, mySS \\ atk)  
    where mySS = myUndockedShips m
          ps = listDockablePlanet m
          enms = listEnemyShips m
          shouldAtk = \s -> length ps == 0  || distance s ( getClosestFromList s enms) < distance s (getClosestFromList s ps)
          atk = filter shouldAtk mySS


allCanDock :: [Ship] -> [Planet] -> [(Ship,Planet)]
allCanDock _ [] = []
allCanDock [] _ = []
allCanDock (s:ss) ps
    | length result > 0 = (s,head result) : allCanDock ss ps
    | otherwise = allCanDock ss ps
    where result = filter (canDock s) ps

restShipFromPlan :: [Ship] -> [(Ship, a)] -> [Ship]
restShipFromPlan [] _ = []
restShipFromPlan (s:allShips) plan
    | s `elem` dShips = restShipFromPlan allShips plan
    | otherwise = s : (restShipFromPlan allShips plan)
        where dShips = map fst plan


explorationDistribute :: [Ship] -> GameMap -> [(Ship,Planet)]
explorationDistribute [] _ = [] 
explorationDistribute (s:ss) m = (s,targetP) : explorationDistribute ss m
    where myPs = listMyPlanet m
          myUnderProdPs = filter (\p -> not (isFull p)) myPs 
          freePs = allFreePlanet m
          targetP
            | length freePs == 0 = getClosestFromList s myPs
            | length myUnderProdPs == 0 = getClosestFromList s freePs
            | otherwise = getClosestFromList s myUnderProdPs


targetPlanToLocationPlan :: Eq a => Entity a => [(Ship, a)] -> [(Ship,Location)]
targetPlanToLocationPlan [] = []
targetPlanToLocationPlan tp = concat $ map multiShipToOneEntity groups
    where groups = groupPlanByTarget tp


multiShipToOneEntity :: Entity a => [(Ship, a)] -> [(Ship,Location)]
multiShipToOneEntity ps = zip ships locations 
    where toLoc = \t rad -> nearbyLocationByRadians t rad
          ships = map fst ps
          locations = zipWith toLoc targets radians
          targets = map snd ps
          ship = fst $ head ps
          radians = seperateRadians $ map (\a -> angleRadians ship a) ships
           
seperateRadians :: [Float] -> [Float]
seperateRadians [] = []
seperateRadians [a] = [a]
seperateRadians (f:s:rest)
  | abs(f-s) > 0.1 = f: (seperateRadians (s:rest))
  | otherwise = f: (seperateRadians (map (+0.1) (s:rest)))


groupPlanByTarget :: Eq a => Entity a => [(Ship, a)] -> ([[(Ship, a)]])
groupPlanByTarget ps = groupBy sameTarget ps 
    where sameTarget = \f s -> (snd f) == (snd s)

attackDistribution :: [Ship] -> GameMap -> [(Ship,Ship)] 
attackDistribution [] _ = []
attackDistribution (s:mySs) m = (s,target) : attackDistribution mySs m
    where target = getClosestFromList s $ listEnemyShips m
          enmSs = listEnemyShips m
          enemy = maximumBy compPriority enmSs
          compPriority = \a b -> compare (attackPriority s a) (attackPriority s b)

attackPriority :: Ship -> Ship -> Float
attackPriority myS enS = -distance myS enS
    where dockScore 
              | dockingStatus enS == Undocked = 0
              | otherwise = 5




