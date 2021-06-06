module MissionInterface(
      showHelp,
      showIntro,
      getCityName,
      fly

) where


import World
import System.IO
import Data.Maybe

--function allows to get name of the city from list by index
getCityName :: Int -> [String] -> String
getCityName place cities = cities !! place

--function returns introduction to game
showIntro :: State -> [String] -> String
showIntro (place, history, world, moves, goal) cities = "Witaj! \n" ++
    "Mamy sytuacje kryzysowa. Musisz dostarczyc tajna przesylke z lotniska w " ++
    getCityName (fromIntegral place ::Int) cities ++
    " na lotnisko w " ++
    getCityName (fromIntegral goal ::Int) cities ++
    ". \n" ++
    "Niestety czasu jest niewiele, a misja moze byc niebezpieczna. Na wykonanie zadania masz 20h. \n" ++
    "Uwazaj! Po drodze mozesz spotkac wrogisz agentow, ktorzy sprobuja nie dopuscic, by przesylka dotarla na miejsce na czas. \n" ++
    "Niestety nasz system informatyczny zostal zhackowany i nie mozemy Ci zapewnic transportu ani wsparcia. Musisz na biezaco planowac swoja podroz, by wykonac zadanie. " ++
    "Twoje 20h zaczyna sie teraz. \n" ++
    "Powodzenia! \n"

--function returns help with informations about mechanics of the game
showHelp = "\npomoc -> wyswietlenie podpowiedzi \n" ++
    "czas -> sprawdzenie, ile czasu jeszcze masz do zakonczenia misji \n" ++
    "cel -> sprawdzenie, dokad musisz dotrzec \n" ++
    "wyjdz -> poddanie sie i skonczenie gry \n"++
    "Po przylocie do kazdego miasta, z wyjatkiem stanu poczatkowego i powtarzania statow, zostanie wyswietlone zadanie do wykonania. Odpowiedz nalezy wpisac korzystajac z malych liter. Za poprawne rozwiazanie mozna czasem otrzymac nagrode, natomiast za zle uzytkownik jest karany. \n" ++
    "Na kazdym lotnisku po wykonaniu zadania (jesli takie sie pojawilo) lub bezposrednio po przemieszczeniu pojawi sie lista dostepnych polaczen lotniczych. W celu wybrania danego polaczenia nalezy podac numer znajdujacy sie przy nazwie miasta. Kazdy lot zabiera 3h z dostepnego czasu. \n" ++
    "\n"

--function returns name of the target city
getTarget :: State -> [String] -> String
getTarget (place, history, world, moves, goal) cities = getCityName (fromIntegral goal ::Int) cities

--function checks if choosen move is possible from current state
checkConnection :: State -> Vertex -> Bool
checkConnection (place, history, world, moves, goal) nextMove = elem nextMove (getMoves (place, history, world, moves, goal))

--function allows to change place if the move is allowed
fly :: State -> Maybe Int -> State
fly (place, history, world, moves, goal) d = case d of
    Nothing -> (place, history, world, moves, goal)
    otherwise -> do
            let intD = fromJust d
            case (checkConnection (place, history, world, moves, goal) (fromIntegral intD :: Integer)) of
                True -> move (place, history, world, moves, goal) (fromIntegral intD ::Integer)
                otherwise -> (place, history, world, moves, goal)
