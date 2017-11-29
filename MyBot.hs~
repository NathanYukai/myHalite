import Hlt.Constants
import Hlt.Entity
import Hlt.GameMap
import Hlt.Navigation
import Hlt.Networking
import Hlt.DistributeShips
import Hlt.Utils

-- | Define the name of our bot.
botName :: String
botName = "AdamBot"

-- | Log to a file
info :: GameMap -> String -> IO ()
info g s = appendFile (show (myId g) ++ "-" ++ botName ++ ".log") (s ++ "\n")

myProductionRate :: [Planet] -> Int
myProductionRate ps = sum $ map production ps

cmd_executeExplore :: [(Ship, Planet)] -> GameMap -> [String]
cmd_executeExplore [] _ = []
cmd_executeExplore ((s,p):rest) m = cmd : (cmd_executeExplore rest m) 
    where cmd = navigateToTarget m maxSpeed s (closestLocationTo s p)


cmd_DockAll :: [(Ship, Planet)] -> [String]
cmd_DockAll [] = []
cmd_DockAll ((s,p):rest) = dock s p : cmd_DockAll rest


-- | The primary function for controlling the game turns.
run :: GameMap -> IO ()
run i = do
    -- Update map
    g <- updateGameMap i
    info g "---NEW TURN---"

    let ss = filter isUndocked (listMyShips g)         -- all undocked Ships of mine
        ps = filter (not . isOwned) (listAllPlanets g) -- all unowned Planets on the map
        dockPlan = allCanDock ss ps
        canNotDockShips = restShipFromPlan ss dockPlan
        explorePlan = explorationDistribute canNotDockShips ps
        allCommand = cmd_DockAll dockPlan ++ cmd_executeExplore explorePlan g  
    
    
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
