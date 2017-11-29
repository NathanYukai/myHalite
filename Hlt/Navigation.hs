-- | Functions for Ship navigation.
module Hlt.Navigation where

import Hlt.Constants
import Hlt.Entity
import Hlt.GameMap

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
moveToTarget s e = thrust s (min (distanceEdges s e) maxSpeed) (angleRadians s e)

-- target parameter is not used for now.
getAvoidAngle :: Ship -> ([Planet],[Ship]) -> Float
getAvoidAngle _ ([],[]) = 0
getAvoidAngle s ([],pobs) = atan $ lenToAvoid/dToObs
    where o = head pobs
          dToObs = distance s o
          lenToAvoid = radius o 
getAvoidAngle s (sobs,_) = atan $ lenToAvoid/dToObs
    where o = head sobs
          dToObs = distance s o
          lenToAvoid = radius o 

navigateToTarget :: Entity a => GameMap -> Float -> Ship -> a -> String
navigateToTarget map spd ship t = thrust ship spd $ a+addAngle
    where d = distance ship t
          a = angleRadians ship t
          obstacles = entitiesBetween map ship t
          addAngle = getAvoidAngle ship obstacles




