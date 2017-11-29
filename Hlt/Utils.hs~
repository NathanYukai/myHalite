module Hlt.Utils where

import Hlt.Constants
import Hlt.GameMap
import Hlt.Entity
import Data.List

listEnemyShips :: GameMap -> [Ship]
listEnemyShips m = listAllShips m \\ listMyShips m

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


