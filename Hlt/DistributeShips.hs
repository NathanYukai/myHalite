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

explorationDistribute :: [Ship] -> [Planet] -> [(Ship,Planet)]
explorationDistribute [] _ = []
explorationDistribute _ [] = []
explorationDistribute (s:ss) ps = (s,p) : explorationDistribute ss restP
    where p = minimumBy comp ps
          restP = ps \\ [p]
          comp = \a b -> compare (distance s a) (distance s b)


