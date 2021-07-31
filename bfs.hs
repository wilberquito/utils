import Data.List
import Data.Maybe
{-
    Graph:
        [
            (1, [2,3]),
            (2, [3]),
            (3, [4, 2]),
            (4, [1])
        ]
-}

g1 :: [(Int, [Int])]
g1 =
  [ (1, [2, 3]),
    (2, [3]),
    (3, [4, 2]),
    (4, [1])
  ]

g2 :: [(Int, [Int])]
g2 =
  [ (1, [2, 3]),
    (3, [4, 2]),
    (2, [3]),
    (4, [1])
  ]

g3 :: [(Int, [Int])]
g3 =
  [ (1, [2, 6]),
    (2, [3, 4]),
    (3, [4]),
    (4, [5]),
    (5, [2, 3]),
    (6, [])
  ]

{-
>>> bfs g1 2
[2,3,4,1]

 1)
    ibfs [2] [2] G
    -> else
        nodo = (2,[3])
        neighbords = [3]
        utils = [3]
        nq = [] ++ [3] = [3]
        visited = [2] ++ [3] = [2, 3]
    2)
    ibfs [2,3] [3] G
    -> else
        nodo = (3, [4,2])
        neighbords = [4,2]
        utils = [4]
        nq = [] ++ 4 = [4]
        nv = [2,3] ++ [4] = [2,3,4]
    3)
    ibfs [2,3,4] [4] G
    -> else
        nodo = (4, [1])
        neighbords = [1]
        utils = [1]
        nq = [] ++ [1] = [1]
        nv = [2,3,4] ++ [1] = [2,3,4,1]
    4)
    ibfs [2,3,4,1] [1] G
    -> else
        nodo = (1,[2,3])
        neighbords = [2,3]
        utils = []
        nq = [] ++ [] = []
        nv = [2,3,4,1] ++ [] = [2,3,4,1]
    5)
    ibfs [2,3,4,1] [] G
    -> if
        [2,3,4,1]

>>> bfs g2 2
[2,3,4,1]

>>> bfs g3 4
[4,5,2,3]
-}

-- (graph, root) => [nodos visitados por orden]
bfs :: Eq a => [(a, [a])] -> a -> [a]
bfs graph nodo = ibfs [nodo] [nodo] graph

-- (visited, queue, graph) => visited
ibfs :: Eq a => [a] -> [a] -> [(a, [a])] -> [a]
ibfs visited queue graph =
  do
    if null queue
      then visited
    else do
        let nodo = fromJust $ find (\x -> fst x == head queue) graph
            neighbors = snd nodo
            utils = [n | n <- neighbors, n `notElem` visited]
            nq = tail queue ++ utils
            nv = visited ++ utils
        ibfs nv nq graph



{-
>>>  hasPath g3 2 4
True
>>> hasPath g3 6 4
False

>>>  hasPath g3 2 5
True

False
-}
-- (graph, n1, n2) => True | False
hasPath :: Eq a => [(a, [a])] -> a -> a -> Bool
hasPath graph n1 n2
    | n1 == n2 = True
    | otherwise =  ibfs2 [n1] [n1] n2 graph


-- (visited, queue, graph, goal) => visited
ibfs2 :: Eq a => [a] -> [a] -> a -> [(a, [a])] -> Bool
ibfs2 _ [] _ _ = False
ibfs2 visited queue goal graph = 
  do
    let nodo = fromJust $ find (\x -> fst x == head queue) graph
        neighbors = snd nodo
        utils = [n | n <- neighbors, n `notElem` visited]
        nq = tail queue ++ utils
        nv = visited ++ utils

    (goal `elem` neighbors) || ibfs2 nv nq goal graph
