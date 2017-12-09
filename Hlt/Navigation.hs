-- | Functions for Ship navigation.
module Hlt.Navigation where

import Hlt.Constants
import Hlt.Entity
import Hlt.GameMap
import Hlt.Utils

-- | Converts angle in radians to degrees and truncates to an int.
radiansToDegrees :: Float -> Int
radiansToDegrees a = mod (round (a * 180 / pi)) 360

-- | Generates a thrust command.
thrust :: Ship -> Float -> Float -> String
thrust s v a = "t " ++ (show $ shipId s) ++ " " ++ (show $ (floor v :: Int)) ++ " " ++ (show $ radiansToDegrees a)

-- | Generates a dock command.
dock :: Ship -> Planet -> String
dock s p = "d " ++ (show $ shipId s) ++ " " ++ (show $ planetId p)

-- | Generates an undock command.
undock :: Ship -> String
undock s = "u " ++ (show $ shipId s)

-- | Move a Ship directly towards a target.
moveToTarget :: Entity a => Ship -> a -> String
moveToTarget s e = thrust s spd (angleRadians s e)
    where dist = distanceEdges s e
          spd = getNotCrashSpd dist maxSpeed

-- target parameter is not used for now.
getAvoidAngle :: Ship -> ([Planet],[Ship]) -> Float
getAvoidAngle _ ([],[]) = 0
getAvoidAngle s ([],pobs) = atan $ lenToAvoid/dToObs
    where o = getClosestFromList s pobs 
          dToObs = distance s o
          lenToAvoid = radius o 
getAvoidAngle s (sobs,_) = atan $ lenToAvoid/dToObs
    where o = getClosestFromList s sobs
          dToObs = distance s o
          lenToAvoid = radius o 

isLocationFree :: GameMap -> Location -> Bool
isLocationFree m l = not $ or $ checkShips ++ checkPlanets
    where bunpToShip = \s -> shipX s == locationX l && shipY s == locationY l
          bunpToPlanet = \p -> distance l p <= radius p + 0.5
          checkShips = map bunpToShip $ listAllShips m
          checkPlanets = map bunpToPlanet $ listAllPlanets m

navigateToTarget :: Entity a => GameMap -> Ship -> a -> String
navigateToTarget map ship t = thrust ship spd $ oa + angle
    where d = distance ship t
          oa = angleRadians ship t
          obstacles = entitiesBetween map ship t
          (spd,angle) = head $ [(s,a) | s <- spdRange, a <- angleRange, works s (a+oa)] ++ [(1,oa)]
          spdRange = [maxSpeed, maxSpeed-0.5 .. 1]
          angleRange = [0,0.1..(pi/2)] 
          works = \s a -> noPnoS $ entitiesBetween map ship (Location (shipX ship + s *cos a) ( shipY ship + s * sin a))
          noPnoS = \(p,s) -> length p + length s  == 0

          

--getAvailableAngleSpd :: 




