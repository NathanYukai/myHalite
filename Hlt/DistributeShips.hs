module Hlt.DistributeShips where

import Hlt.Entity

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
explorationDistribute (s:ss) (p:ps) = (s, p) : explorationDistribute ss ps

