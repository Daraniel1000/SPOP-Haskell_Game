module Main where
import World
import Interface
import Quest
import Text.Read


--go through the basic map
goThrough :: State->Bool
goThrough state | getMoves state == [] = False
                | isWin state          = True
                | isLose state         = False
                | otherwise            = goThrough (move state (last (getMoves state)))

--go through the basic map incorrectly
getLost :: State->Bool
getLost state   | getMoves state == [] = False
                | isWin state          = True
                | isLose state         = False
                | otherwise            = getLost (move state (head (getMoves state)))

--map, see world.jpg
worldMap = [(0, [1, 2, 3]), (1, [0, 2, 4]), (2, [0, 1, 6]), (3, [0, 6]), (4, [1, 5]), (5, [4]), (6, [2, 3, 7]), (7, [6])]

cityNames = ["Warszawa", "Londyn", "Berlin", "Praga", "Skopje", "Madryt", "Helsinki", "Ryga"]

startState = (0, [], worldMap, 7, 7)


main :: IO ()
main = do
    putStr (showIntro startState cityNames)
    showPosibilities startState cityNames True
    return ()


doQuest :: State -> [String] -> IO State
doQuest (place, history, world, moves, goal) cities = do
    putStrLn (showQuest place)
    solution <- getLine
    case solution of
        "pomoc" -> do
            putStr showHelp
            doQuest (place, history, world, moves, goal) cities
        "czas" -> do
            putStr ("Pozostalo Ci " ++ (show moves) ++ " ruchow. Spiesz sie!\n \n")
            doQuest (place, history, world, moves, goal) cities
        "cel" -> do
            putStr ("Musisz dotrzec do " ++ (getCityName (fromIntegral goal ::Int) cities) ++ ". \n \n")
            doQuest (place, history, world, moves, goal) cities
        "wyjdz" -> do
            putStr ("Dzieki za gre. Do zobaczenia przy kolejnej misji! \n")
            return (-1, history, world, moves, goal)
        otherwise -> do
            if solution == ( checkSolution place )
                then do
                    let newState = correctSolution (place, history, world, moves, goal)
                    putStr (correctSolutionComment place)
                    return newState
                else do
                    let newState = wrongSolution (place, history, world, moves, goal)
                    putStr (wrongSolutionComment place)
                    return newState


showCities :: [String]-> [Vertex] -> IO ()
showCities cities indices = do
    putStrLn (show (head indices) ++
        " - " ++
        getCityName (fromIntegral (head indices) ::Int) cities)
    if length indices > 1
        then showCities cities (tail indices)
        else return ()

showGreeting :: Vertex -> [String] -> IO ()
showGreeting place cities = putStr ("Witaj w " ++ (getCityName (fromIntegral place ::Int) cities) ++ ".\n")

checkGameState:: State -> Bool
checkGameState (place, history, world, moves, goal) = place == goal || moves < 1

showPosibilities :: State -> [String] -> Bool -> IO ()
showPosibilities (place, history, world, moves, goal) cities repeated = do
    if checkGameState (place, history, world, moves, goal) == True
        then do
            if place == goal
                then do
                    putStr ("ZWYCIESTWO! Udalo Ci sie pokonac wszystkie przeciwnosci i dostarczyc przesylke na czas. Dziekujemy. \n")
                    return ()
                else do
                    putStr ("PORAZKA! Znowu zlo zwyciezylo, ale mamy nadzieje, ze nastepnym razem pojdzie lepiej. \n")
                    return ()
        else do
            showGreeting place cities
            if repeated == False
                then do
                    afterTaskState <- doQuest (place, history, world, moves, goal) cities
                    showPosibilities afterTaskState cities True
                else do
                    putStr "Z obecnego polozenia mozesz doleciec do: \n"
                    let vertexList =  getMoves (place, history, world, moves, goal)
                    showCities cities vertexList
                    putStr "Podaj numer miejsca, do ktorego chcesz sie teraz udac \n"
                    decision <- getLine
                    case decision of
                        "pomoc" -> do
                            putStr showHelp
                            showPosibilities (place, history, world, moves, goal) cities True
                        "czas" -> do
                            putStr ("Pozostalo Ci " ++ (show moves) ++ " ruchow. Spiesz sie!\n \n")
                            showPosibilities (place, history, world, moves, goal) cities True
                        "cel" -> do
                            putStr ("Musisz dotrzec do " ++ (getCityName (fromIntegral goal ::Int) cities) ++ ". \n \n")
                            showPosibilities (place, history, world, moves, goal) cities True
                        "wyjdz" -> do
                            putStr ("Dzieki za gre. Do zobaczenia przy kolejnej misji! \n")
                            return ()
                        otherwise -> do
                            let newState = fly (place, history, world, moves, goal) (readMaybe decision :: Maybe Int)
                            if newState == (place, history, world, moves, goal)
                                then showPosibilities (place, history, world, moves, goal) cities True
                                else showPosibilities newState cities False
