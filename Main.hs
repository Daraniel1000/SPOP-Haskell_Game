import World

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

startState = (0, [], worldMap, 7, 7)
