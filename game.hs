--Vertex is a number identifier
type Vertex = Int
--Node is identified by its number and has an array of neighbours
type Node = (Vertex, [Vertex])
--Graph is an array of nodes
type Graph = [Node]
--The current state contains the current vertex identifier, an array containing the history for going back, the world graph, the current remaining moves and the goal identifier
type State = (Vertex, [Vertex], Graph, Int, Vertex)

--get a node from a graph by vertex identifier
getNode :: Vertex->Graph->Node
getNode v ((i, n) : rest) | v==i        = (i, n)
                          | otherwise   = getNode v rest
getNode v [] = (-1, [])

--map, see world.jpg
world = [(0, [0, 1, 2, 3]), (1, [0, 2, 4]), (2, [0, 1, 6]), (3, [0, 6]), (4, [1, 5]), (5, [4]), (6, [2, 3, 7]), (7, [6])]

startState = (0, [], world, 7, 7)
