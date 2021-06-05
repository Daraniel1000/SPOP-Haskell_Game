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

questTexts = ["Warszawa - q", "Krakow - q", "Berlin - q", "Amsterdam - q", "Paryz - q", "Bruksela - q", "Wieden - q", "Londyn - q", "Ateny - q", "Moskwa - q", "Kair - q", "Rijad - q", "Nowe Delhi - q", "Hanoi - q", "Sydney - q", "Tokio - q", "Pekin - q", "Seul - q", "Nowy Jork - q"]
questSolutions = ["Warszawa", "Krakow", "Berlin", "Amsterdam", "Paryz", "Bruksela", "Wieden", "Londyn", "Ateny", "Moskwa", "Kair", "Rijad", "Nowe Delhi", "Hanoi", "Sydney", "Tokio", "Pekin", "Seul", "Nowy Jork"]
questSolvedComments = ["Warszawa - g", "Krakow - g", "Berlin - g", "Amsterdam - g", "Paryz - g", "Bruksela - g", "Wieden - g", "Londyn - g", "Ateny - g", "Moskwa - g", "Kair - g", "Rijad - g", "Nowe Delhi - g", "Hanoi - g", "Sydney - g", "Tokio - g", "Pekin - g", "Seul - g", "Nowy Jork - g"]
questFailedComments = ["Warszawa", "Krakow", "Berlin", "Amsterdam", "Paryz", "Bruksela", "Wieden", "Londyn", "Ateny", "Moskwa", "Kair", "Rijad", "Nowe Delhi", "Hanoi", "Sydney", "Tokio", "Pekin", "Seul", "Nowy Jork"]


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
