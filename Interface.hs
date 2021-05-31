module Interface (
      showHelp,
      showIntro,
      getCityName,
      fly

) where


import World
import System.IO
import Data.Maybe


getCityName :: Int -> [String] -> String
getCityName place cities = cities !! place


showIntro :: State -> [String] -> String
showIntro (place, history, world, moves, goal) cities = "Witaj! \n" ++
    "Mamy sytuacje kryzysowa. Musisz dostarczyc tajna przesylke z " ++
    getCityName (fromIntegral place ::Int) cities ++
    " do " ++
    getCityName (fromIntegral goal ::Int) cities ++
    ". \n" ++
    "Niestety czasu jest niewiele, a misja moze byc niebezpieczna. Na wykonanie zadania masz 20h. \n" ++
    "Uwazaj! Po drodze mozesz spotkac wrogisz agentow, ktorzy sprobuja nie dopuscic, by przesylka dotarla na miejsce na czas. \n" ++
    "Niestety nasz system informatyczny zostal zhackowany i nie mozemy Ci zapewnic transportu ani wsparcia. Musisz na biezaco planowac swoja podroz, by wykonac zadanie. " ++
    "Twoje 20h zaczyna sie teraz. \n" ++
    "Powodzenia! \n"

showHelp = "pomoc -> wyswietlenie podpowiedzi \n" ++
    "W celu przelotu do danego miasta nalezy wpisac numer, ktory sie przy nim wyswietla, nie mozesz poleciec do miasta nie znajdujacego sie na liscie \n" ++
    "Na lotniskach czekaja Cie rozne przeszkody lub decyzje do podjecia. Zle rozwiazanie powoduje negatywne konsekwencja, a dobre nagrody. \n" ++
    "czas -> sprawdzenie, ile czasu jeszcze masz do zakonczenia misji \n" ++
    "cel -> sprawdzenie, dokad musisz dotrzec \n" ++
    "wyjdz -> poddanie sie i skonczenie gry \n \n"

getTarget :: State -> [String] -> String
getTarget (place, history, world, moves, goal) cities = getCityName (fromIntegral goal ::Int) cities

checkConnection :: State -> Vertex -> Bool
checkConnection (place, history, world, moves, goal) nextMove = elem nextMove (getMoves (place, history, world, moves, goal))

fly :: State -> Maybe Int -> State
fly (place, history, world, moves, goal) d = case d of
    Nothing -> (place, history, world, moves, goal)
    otherwise -> do
            let intD = fromJust d
            case (checkConnection (place, history, world, moves, goal) (fromIntegral intD :: Integer)) of
                True -> move (place, history, world, moves, goal) (fromIntegral intD ::Integer)
                otherwise -> (place, history, world, moves, goal)
