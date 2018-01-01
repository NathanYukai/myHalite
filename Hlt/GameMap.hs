-- | A data type representing the game map at a particular frame, and functions that operate on it.
module Hlt.GameMap where

import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Data.List
import Hlt.Entity

-- | A GameMap contains the current frame of a Halite game.
data GameMap = GameMap { myId :: PlayerId
                       , width :: Int
                       , height :: Int
                       , allPlayers :: Map.Map PlayerId Player
                       , allPlanets :: Map.Map PlanetId Planet
                       }
              deriving (Show)

-- | Return a list of all Planets.
listAllPlanets :: GameMap -> [Planet]
listAllPlanets g = Map.elems $ allPlanets g

-- | Return a list of all Ships.
listAllShips :: GameMap -> [Ship]
listAllShips g = concat $ map (Map.elems . ships) (Map.elems $ allPlayers g)



listEnemyPlanet :: GameMap -> [Planet]
listEnemyPlanet m = filter isMine $ listAllPlanets m
    where isMine = \p -> isOwned p && ( (Maybe.fromJust (planetOwner p)) /= myId m)



-- | Checks if any of the given Entities are in between two Entities.
entitiesBetweenList :: Entity a => Entity b => Entity c => [a] -> b -> c -> [a]
entitiesBetweenList l e0 e1 = filter (\e -> isSegmentCircleCollision e0 e1 e) $ filter (\e -> notEqual e e0 && notEqual e e1) l

-- | Checks if there are any Entities between two Entities.
entitiesBetween :: Entity a => Entity b => GameMap -> a -> b -> ([Planet],[Ship]) 
entitiesBetween g e1 e2 = (entitiesBetweenList (listAllPlanets g) e1 e2 , (entitiesBetweenList (listAllShips g) e1 e2))
