import Hlt.Constants
import Hlt.Entity
import Hlt.GameMap
import Hlt.Navigation
import Hlt.Networking
import Hlt.DistributeShips
import Hlt.Utils
import Data.List

-- | Define the name of our bot.
botName :: String
botName = "NSBot"

-- | Log to a file
info :: GameMap -> String -> IO ()
info g s = appendFile (show (myId g) ++ "-" ++ botName ++ ".log") (s ++ "\n")

cmd_executeLocationPlan :: [(Ship,Location)] -> GameMap -> [String]
cmd_executeLocationPlan [] m = []
cmd_executeLocationPlan ((s,e):rest) m = cmd  : cmd_executeLocationPlan rest m
    where cmd = navigateToTarget m s e


cmd_DockAll :: [(Ship, Planet)] -> [String]
cmd_DockAll [] = []
cmd_DockAll ((s,p):rest) = dock s p : cmd_DockAll rest


-- | The primary function for controlling the game turns.
run :: GameMap -> IO ()
run i = do
    -- Update map
    g <- updateGameMap i
--    info g "---NEW TURN---"

    let distribution = distributeExploreAttackGather g
        exploreSs = explore distribution
        atkSs = attacks distribution

        ps = listAllPlanets g
        enmSs = listEnemyShips g
        dockPlan = allCanDock exploreSs ps
        canNotDockShips = restShipFromPlan exploreSs dockPlan
        explorePlan = explorationDistribute canNotDockShips g 
        attackPlan = attackDistribution atkSs g

        exploreCmd = cmd_executeLocationPlan (targetPlanToLocationPlan explorePlan) g
        attackCmd = cmd_executeLocationPlan (targetPlanToLocationPlan attackPlan) g
        allCommand = cmd_DockAll dockPlan ++ exploreCmd ++ attackCmd
    
        
    info g $ "------DEBUG---" ++ intercalate "\n" allCommand

    -- Send commands to move each Ship to the first empty Planet
    sendCommands allCommand

    -- Go to next turn
    run g

-- | Main function where we initialize our bot and call the run function.
main :: IO ()
main = do
    i <- initialGameMap

    -- You can analyse the initial map (i) here, 60 seconds time limit

    sendString botName
    info i ("Initialized bot " ++ botName)
    run i
