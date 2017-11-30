module Hlt.DistributeShips where

import Hlt.Entity
import Data.List

allCanDock :: [Ship] -> [Planet] -> [(Ship,Planet)]
allCanDock ss ps = [(s,p) | s <- ss, p <-ps , canDock s p]

restShipFromPlan :: [Ship] -> [(Ship,Planet)] -> [Ship]
restShipFromPlan [] _ = []
restShipFromPlan (s:allShips) dockPlan
    | s `elem` dShips = restShipFromPlan allShips dockPlan
    | otherwise = s : restShipFromPlan allShips dockPlan
        where dShips = map fst dockPlan

getClosestFromList :: Entity a => Entity b => a -> [b] -> b
getClosestFromList e1 list = minimumBy comp list
    where comp = \a b -> compare (distance e1 a) (distance e1 b)

explorationDistribute :: [Ship] -> [Planet] -> [(Ship,Planet)] explorationDistribute [] _ = [] explorationDistribute _ [] = []
explorationDistribute (s:ss) ps = (s,p) : explorationDistribute ss restP
    where p = getClosestFromList s ps
          restP = ps \\ [p]



-- attack nearby docking enemy ship first, then nearby freeShip
attackPlan :: Entity a => [Ship] -> GameMap -> [(Ship,a)] 
attackPlan (s:mySs) map = (s,target) : attackPlan mySs map
    where enmSs = listEnemyShips map
          target = closestLocationTo s enemy
          enemy = minimumBy 




