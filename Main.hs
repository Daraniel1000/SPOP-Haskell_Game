module Main where
import World
import Interface
import Quest
import Text.Read


--map, see world.jpg
worldMap = [(0, [1, 2, 4]), (1, [0, 6, 8]), (2, [0, 7, 12]), (3, [4, 9]), (4, [0, 3, 5]), (5, [4, 9, 16]), (6, [1, 8, 14]), (7, [2, 16, 18]), (8, [1, 6, 10]), (9, [3, 5, 16]), (10, [8, 11, 12]), (11, [10, 13]), (12, [2, 10, 13]), (13, [11, 13]), (14, [6, 15, 17]), (15, [14, 17, 18]), (16, [5, 7, 9]), (17, [5, 14, 15]), (18, [7, 9, 15])]

--names of the cities
cityNames = ["Warszawa", "Krakow", "Berlin", "Amsterdam", "Paryz", "Bruksela", "Wieden", "Londyn", "Ateny", "Moskwa", "Kair", "Rijad", "Nowe Delhi", "Hanoi", "Sydney", "Tokio", "Pekin", "Seul", "Nowy Jork"]

--names of the cities in different form
cityNamesWelcomePack = ["Warszawie", "Krakowie", "Berlinie", "Amsterdamie", "Paryzu", "Brukseli", "Wiedniu", "Londynie", "Atenach", "Moskwie", "Kairze", "Rijadzie", "Nowym Delhi", "Hanoi", "Sydney", "Tokio", "Pekinie", "Seulu", "Nowym Jorku"]

--first state of the game, player starts the game in Warszawa, with 20 hours to go to Tokio
startState = (0, [], worldMap, 20, 15)

--function allows to start the game and shows introduction to the mission
main :: IO ()
main = do
    putStr (showIntro startState cityNamesWelcomePack)
    showPosibilities startState cityNames cityNamesWelcomePack True
    return ()

--function with provides interface for completing quests during the mission
doQuest :: State -> [String] -> IO State
doQuest (place, history, world, moves, goal) cities = do
    putStr ("\n" ++ showQuest place ++ "\n")
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
                    putStr ("\n" ++ correctSolutionComment place ++ "\n")
                    return newState
                else do
                    let newState = wrongSolution (place, history, world, moves, goal)
                    putStr ("\n" ++ wrongSolutionComment place ++ "\n")
                    return newState

--function displays names and numbers of the cities from given indices
showCities :: [String]-> [Vertex] -> IO ()
showCities cities indices = do
    putStrLn (show (head indices) ++
        " - " ++
        getCityName (fromIntegral (head indices) ::Int) cities)
    if length indices > 1
        then showCities cities (tail indices)
        else return ()

--function showing welcome message in every city
showGreeting :: Vertex -> [String] -> IO ()
showGreeting place cities = putStr ("\nWitaj w " ++ (getCityName (fromIntegral place ::Int) cities) ++ ".\n")

--function checks if the games reaches one of finish states
checkGameState:: State -> Bool
checkGameState (place, history, world, moves, goal) = place == goal || moves < 1

--function with allows to do quests, fly to another city and ends when game is finished
showPosibilities :: State -> [String] -> [String] -> Bool -> IO ()
showPosibilities (place, history, world, moves, goal) cities citiesWelcome repeated = do
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
            showGreeting place citiesWelcome
            if repeated == False
                then do
                    (newPlace, newHistory, newWorld, newMoves, newGoal) <- doQuest (place, history, world, moves, goal) cities
                    if newPlace == -1
                        then do
                            return ()
                        else do
                            showPosibilities (newPlace, newHistory, newWorld, newMoves, newGoal) cities citiesWelcome True
                else do
                    putStr "Z obecnego polozenia mozesz doleciec do: \n"
                    let vertexList =  getMoves (place, history, world, moves, goal)
                    showCities cities vertexList
                    putStr "Podaj numer miejsca, do ktorego chcesz sie teraz udac \n"
                    decision <- getLine
                    case decision of
                        "pomoc" -> do
                            putStr showHelp
                            showPosibilities (place, history, world, moves, goal) cities citiesWelcome True
                        "czas" -> do
                            putStr ("Pozostalo Ci " ++ (show moves) ++ "h. Spiesz sie!\n \n")
                            showPosibilities (place, history, world, moves, goal) cities citiesWelcome True
                        "cel" -> do
                            putStr ("Musisz dotrzec do " ++ (getCityName (fromIntegral goal ::Int) cities) ++ ". \n \n")
                            showPosibilities (place, history, world, moves, goal) cities citiesWelcome True
                        "wyjdz" -> do
                            putStr ("Dzieki za gre. Do zobaczenia przy kolejnej misji! \n")
                            return ()
                        otherwise -> do
                            let (newPlace, newHistory, newWorld, newMoves, newGoal) = fly (place, history, world, moves, goal) (readMaybe decision :: Maybe Int)
                            if (newPlace, newHistory, newWorld, newMoves, newGoal) == (place, history, world, moves, goal)
                                then do
                                    showPosibilities (place, history, world, moves, goal) cities citiesWelcome True
                                else do
                                    if elem newPlace history
                                        then do
                                            showPosibilities (newPlace, newHistory, newWorld, newMoves, newGoal) cities citiesWelcome True
                                        else do
                                            showPosibilities (newPlace, newHistory, newWorld, newMoves, newGoal) cities citiesWelcome False
