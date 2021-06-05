module Quest (
      showQuest,
      checkSolution,
      correctSolution,
      correctSolutionComment,
      wrongSolution,
      wrongSolutionComment

) where


import World
import System.IO
import Data.Maybe

questTexts = ["0 Co tam stoi?", "1 Leloslsoels", "2 ABCDACA", "3 srori", "4 iec", "5 bobo", "6 co sie gapisz?", "7 leszczuu"]
questSolutions = ["Warszawa", "Londyn", "Berlin", "Praga", "Skopje", "Madryt", "Helsinki", "Ryga"]
questSolvedComments = ["Waw - good", "Lon - good", "Ber - good", "Pra - good", "Sko- good", "Mad - good", "Hel - good", "Ryg - good"]
questFailedComments = ["Waw - fail", "Lon - fail", "Ber - fail", "Pra - fail", "Sko- fail", "Mad - fail", "Hel - fail", "Ryg - fail"]


showQuest :: Integer -> String
showQuest place = "Twoje zadanie: " ++ questTexts !! (fromIntegral place ::Int)

checkSolution :: Integer -> String
checkSolution place = questSolutions !! (fromIntegral place ::Int)

correctSolution :: State -> State
correctSolution (place, history, world, moves, goal) = (place, history, world, moves, goal)

correctSolutionComment :: Integer -> String
correctSolutionComment place = questSolvedComments !! (fromIntegral place ::Int) ++ "\n"

wrongSolution :: State -> State
wrongSolution (place, history, world, moves, goal) = (place, history, world, moves, goal)

wrongSolutionComment :: Integer -> String
wrongSolutionComment place = questFailedComments !! (fromIntegral place ::Int) ++ "\n"
