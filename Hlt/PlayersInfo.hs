module Hlt.PlayersInfo where

import Hlt.GameMap
import Hlt.Entity
import Data.List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

numberOfPlayers :: GameMap -> Int
numberOfPlayers gmap = Map.size $ allPlayers gmap 

calcPlayerRank :: GameMap -> PlayerId -> Int
calcPlayerRank gmap pId = Maybe.fromJust $ elemIndex myScore $ sort allScores
  where allScores = map (calcPlayerScore gmap) allIds
        allIds = Map.keys $ allPlayers gmap
        myScore = calcPlayerScore gmap $ myId gmap

calcPlayerScore :: GameMap -> PlayerId -> Int
calcPlayerScore gmap pId = (length pShips) + (sum $ map production planets)
  where planets = listPlayerPlanets gmap pId
        pShips = listPlayerShips gmap pId

listPlayerPlanets :: GameMap -> PlayerId -> [Planet]
listPlayerPlanets gmap pId = filter isHis $ listAllPlanets gmap
  where isHis = \p -> isOwned p && ( (Maybe.fromJust (planetOwner p)) == pId )

listPlayerShips :: GameMap -> PlayerId -> [Ship]
listPlayerShips gmap pId = Map.elems $ ships $ Maybe.fromJust $ Map.lookup pId (allPlayers gmap)




listMyShips :: GameMap -> [Ship]
listMyShips gmap = listPlayerShips gmap $ myId gmap

listMyPlanet :: GameMap -> [Planet]
listMyPlanet gmap= listPlayerPlanets gmap$ myId gmap

listEnemyShips :: GameMap -> [Ship]
listEnemyShips m = listAllShips m \\ listMyShips m

myUndockedShips :: GameMap -> [Ship]
myUndockedShips m = filter (not . isDocked) $ listMyShips m
