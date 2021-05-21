--Vertex is a number identifier
type Vertex = Integer
--Node is identified by its number and has an array of neighbours
type Node = (Vertex, [Vertex])
--Graph is an array of nodes
type Graph = [Node]
--The current state contains the current vertex identifier, an array containing the history for going back, the world graph, the current remaining moves and the goal identifier
type State = (Vertex, [Vertex], Graph, Integer, Vertex)

--get a node from a graph by vertex identifier
getNode :: Vertex->Graph->Node
getNode v [] = (-1, [])
getNode v ((i, n) : rest) | v==i        = (i, n)
                          | otherwise   = getNode v rest

--get node's neighbour array
neighbours :: Node->[Vertex]
neighbours (_, neigh) = neigh

--set a node's neighbour array
setNeighbours (v, _) newNeigh = (v, newNeigh)

--change a node in the graph
changeNode :: Node->Graph->Graph
changeNode _ []                       	    = []
changeNode (v, neigh) ((i, n) : rest) | v == i    = (v, neigh) : rest
                                      | otherwise = (i, n) : (changeNode (v, neigh) rest)

--move in graph
move :: State->Vertex->State
move (place, history, world, moves, goal) goto = (goto, (place:history), world, moves - 1, goal)

--add time
addTime :: State->Integer->State
addTime (p, h, w, moves, g) toAdd = (p, h, w, moves + toAdd, g)

--check if victory state is achieved
isWin :: State->Bool
isWin (place, _, _, _, goal) | place==goal = True
                             | otherwise   = False
                             
--check if loss state is achieved
isLose :: State->Bool
isLose (_, _, _, moves, _) | moves<1   = True
                           | otherwise = False
                            
--list possible moves
--if returns [], node has not been found
getMoves :: State->[Vertex]
getMoves (place, _, world, _, _) = neighbours (getNode place world)


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
