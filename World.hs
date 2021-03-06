module World where
--Vertex is a number identifier
type Vertex = Integer
--Node is identified by its number and has an array of neighbours
type Node = (Vertex, [Vertex])
type Edge = (Vertex, Vertex)
--Graph is an array of nodes
type Graph = [Node]
--The current state contains the current vertex identifier, an array containing the history for going back, the world graph, the current remaining moves and the goal identifier
type State = (Vertex, [Vertex], Graph, Integer, Vertex)

--getters
getPos :: State->Vertex
getPos (p, _, _, _, _) = p
getHistory :: State->[Vertex]
getHistory (_, h, _, _, _) = h
getWorld :: State->Graph
getWorld (_, _, w, _, _) = w
getTime :: State->Integer
getTime (_, _, _, t, _) = t
getGoal :: State->Vertex
getGoal (_, _, _, _, g) = g

--setters
setPos :: State->Vertex->State
setPos (p, h, w, t, g) newp = (newp, h, w, t, g)
setWorld :: State->Graph->State
setWorld (p, h, w, t, g) neww = (p, h, neww, t, g)
setTime :: State->Integer->State
setTime (p, h, w, t, g) newt = (p, h, w, newt, g)
setGoal :: State->Vertex->State
setGoal (p, h, w, t, g) newg = (p, h, w, t, newg)

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
changeNode _ [] = []
changeNode (v, neigh) ((i, n) : rest) | v == i    = (v, neigh) : rest
                                      | otherwise = (i, n) : (changeNode (v, neigh) rest)

--add neighbour to node
addNeighbour :: Vertex->Vertex->Graph->Graph
addNeighbour _ _ [] = []
addNeighbour v n world | n `elem` (neighbours (getNode v world)) = world
                       | otherwise           = changeNode (v, n : (neighbours (getNode v world))) world

--add edge
addEdge :: Edge->Graph->Graph
addEdge _ [] = []
addEdge (v1, v2) world = addNeighbour v1 v2 (addNeighbour v2 v1 world)

--move in graph
move :: State->Vertex->State
move (place, history, world, moves, goal) goto = (goto, (place:history), world, moves - 3, goal)

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
