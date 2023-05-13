{- |
Module      :  Graphs
Description :  Implementation of centroid decomposition, bridge finding and SCC.
Copyright   :  Dominik Farhan
License     :  MIT License

Maintainer  :  dominik(salamander)whizzmot.dev
Stability   :  experimental
Portability :  portable 

Basic implementation of various graph algorithms.
Strongly connected components -- 
    based on Kosaraju's algorithm, assumes that the given graph is a DAG.
Centroid decomposition of a tree --
    calculates centroid tree, it is guaranteed that the tree depth is no more than O(log n) 
    and a lot of algorithms can then work with the tree doing quieries more efficiently.
Bridge and articulation finding --
    finds bridges or articulation based on the algorithms by Robert Tarjan that uses time of entry
    to the node and its 'low' value.
-}

import Control.Monad.ST
import Data.Array
import Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

type Table a = Array Char a
type Graph = Table [Char]
type Edge = (Char, Char)
type Bounds = (Char, Char)
type Forest a = [Tree a]
type ForestTinLow a b c = [TreeTinLow a b c]

data Tree a = Node a (Forest a) deriving (Show)

data TreeTinLow a b c = NodeTinLow a b c (ForestTinLow a b c) deriving (Show)

vertices :: Graph -> [Char]
vertices = indices

edges:: Graph -> [Edge]
edges g = [(v, u) | v <- vertices g, u <- g!v]

buildGraph :: Bounds -> [Edge] -> Graph
buildGraph bounds neighbors = accumArray (flip (:)) [] bounds neighbors

buildUndirectedGraph :: Bounds -> [Edge] -> Graph
buildUndirectedGraph bounds neighbors = buildGraph bounds (neighbors ++ [(b, a) | (a, b) <- neighbors])

-- Reverses all edges in a graph
transpose:: Graph -> Graph
transpose g = buildGraph (bounds g) ([(a, b) | (b, a) <- edges g])


removeVertex:: Graph -> Char -> Graph
removeVertex g v = buildGraph (bounds g) [(a, b) | (a, b) <- edges g, a /= v, b /= v]

-- Define VertexSet and methods to work with it
type VertexSet = Set.Set Char

emptyVertexSet :: VertexSet
emptyVertexSet = Set.empty

isVertexVisited :: Char -> VertexSet -> Bool
isVertexVisited v vertexSet = Set.member v vertexSet

addVisitedVertex :: Char -> VertexSet -> VertexSet
addVisitedVertex vertex vertexSet = Set.insert vertex vertexSet

type VertexValMap = Map.Map Char Int

emptyVertexValMap :: VertexValMap
emptyVertexValMap = Map.empty

addVertexVal :: Char -> Int -> VertexValMap -> VertexValMap
addVertexVal vertex size vertexValMap = Map.insert vertex size vertexValMap

getVertexVal :: Char -> VertexValMap -> Int
getVertexVal vertex vertexValMap = vertexValMap Map.! vertex

degree:: Graph -> Char -> Int
degree g v = length (g!v)

calculateSubtreeSizes :: Graph -> Char -> VertexValMap -> Char -> VertexValMap
calculateSubtreeSizes graph parent subtreeMap root
    | parent /= root && degree graph root == 1 = addVertexVal root 1 subtreeMap
    | otherwise = 
        let children = [ch | ch <- graph!root, ch /= parent]
            subtreeMap2 = foldl (calculateSubtreeSizes graph root) subtreeMap children
            subtreeSizes = map (\x -> getVertexVal x subtreeMap2) children
            subtreeSize = 1 + (sum subtreeSizes)
        in addVertexVal root subtreeSize subtreeMap2

-- Prints the tree in a human-readable format
iterateTreeRecursive :: Tree Char -> [Char]
iterateTreeRecursive (Node v subtrees) =
  v : concatMap iterateTreeRecursive subtrees

-- Prints the forst in a human-readable format by calling iterateTreeRecursive on each tree.
iterateForestRecursive :: Forest Char -> [Char]
iterateForestRecursive f = concatMap iterateTreeRecursive f

-- Generates list of trees representing the DFS forest
dfsTree :: Graph -> Char -> Forest Char
dfsTree graph startVertex = fst (generate graph startVertex emptyVertexSet)

-- Generates the DFS forest
generate :: Graph -> Char -> VertexSet -> (Forest Char, VertexSet)
generate graph v visitedSet = 
    if isVertexVisited v visitedSet
      then 
        ([Node v []], visitedSet)
      else 
        let updatedSet = addVisitedVertex v visitedSet
            (children, vs_back) = (dfsns (graph!v) updatedSet)
        in ([Node v children], vs_back)
    where
        -- Processes ancestors of the current vertex
        dfsns :: [Char] -> VertexSet -> (Forest Char, VertexSet)
        dfsns [] vs = ([], vs)
        dfsns (x:xs) ns =
            if isVertexVisited x ns
            then dfsns xs ns
            else 
                let (forest, new_visited) = generate graph x ns
                    (forest2, new_visited2) = dfsns xs new_visited
                in (forest ++ forest2, new_visited2)

-- Generates the postorder traversal given a DFS tree.
postOrderTree :: Tree Char -> [Char]
postOrderTree (Node v subtrees) = 
    (concatMap postOrderTree subtrees) ++ [v]

-- Generates the postorder traversal for each tree in a given DFS forest.
postOrder :: Forest Char -> [Char]
postOrder f = concat [postOrderTree t | t <- f]

-- Calculates topological ordering of a graph by calling postOrder on the DFS forest.
-- Assumes that the graph is a DAG.
topoSort :: Graph -> [Char]
topoSort g = reverse (postOrder (dfsTree g ((vertices g) !! 0)))

-- Calculates strongly connected components of a graph.
-- Assumes that the graph is a DAG.
scc :: Graph -> [String]
scc g = 
    let transposed = transpose g
    in dfsAll transposed (topoSort g) emptyVertexSet []

-- Calculates the set of vertices reachable from a given vertex.
dfs::Graph -> Char -> VertexSet-> VertexSet
dfs g v visitedSet = 
    if v `isVertexVisited` visitedSet
    then
        visitedSet
    else
        foldr dfs' new_visited (g!v)
        where
            new_visited = addVisitedVertex v visitedSet
            dfs' :: Char -> VertexSet -> VertexSet
            dfs' v vs = dfs g v vs

-- Calculates the set of vertices reachable from a given list of vertices.
-- Each vertex can be reached only once. If there is a vertex that was already met, it is ignored.
dfsAll :: Graph -> [Char] -> VertexSet -> [String] -> [String]
dfsAll g [] _ components = components
dfsAll g (x:xs) visitedSet components = 
    if x `isVertexVisited` visitedSet
    then
        dfsAll g xs visitedSet components
    else
        let visitedSet2 = dfs g x visitedSet
            diff = visitedSet2 `Set.difference` visitedSet
            new_components = components ++ [Set.toList diff]
        in dfsAll g xs visitedSet2 new_components


-- Given a graph (tree), a vertex v, a vertex parent, and the size of the tree finds the centroid of the tree.
-- If v is the root of the tree, then parent is set to v.
-- VertexValMap should be empty when this function is first called.
findCentroid :: Graph -> Char -> Char -> Int -> VertexValMap -> (Char, String)
findCentroid g v parent n subtreeMap
    | g!v == "" = (v, "")
    | otherwise = 
        let (u, sz) = maxNeighborSize g v parent
        in if sz > n `div` 2
        then
            findCentroid g u v n subtreeMap
        else
            (v, g!v)
        where maxNeighborSize :: Graph -> Char -> Char -> (Char, Int)
              maxNeighborSize g v parent = 
                let neighbors = g!v
                    neighbors2 = filter (\x -> x /= parent) neighbors
                    neighborsSizePairs = [(x, getVertexVal x subtreeMap) | x <- neighbors2]
                in foldl (\(x, sz) (u, max_sz) -> if sz > max_sz then (x, sz) else (u, max_sz)) (' ', 0) neighborsSizePairs

-- Returns the centroid tree of a given graph.
centroidDecomposition :: Graph -> Tree Char
centroidDecomposition g = centroidDecomposition' g ((vertices g) !! 0)

-- Returns the centroid tree of a given graph.
-- Calculation start from the vertex v (the root) however it is ok to root the tree at any vertex.
centroidDecomposition' :: Graph -> Char -> Tree Char
centroidDecomposition' g v = 
    let subtreeMap = calculateSubtreeSizes g v emptyVertexValMap v
        (centroid, neighbors) = findCentroid g v v (getVertexVal v subtreeMap) subtreeMap
        splitted_g = removeVertex g centroid
    in if neighbors == []
    then
        Node centroid []
    else
        Node centroid [centroidDecomposition' splitted_g x | x <- neighbors]

-- Takes a Tree and returns TreeTinLow where tin and low values are set to 1000000.
treeToTinLow:: Tree Char -> TreeTinLow Char Int Int
treeToTinLow (Node a []) = NodeTinLow a 1000000 100000 []
treeToTinLow (Node a children) = NodeTinLow a 1000000 100000 [treeToTinLow x | x <- children]

-- Sets time entries to nodes as if the tree was a result of DFS traversal.
setTins:: TreeTinLow Char Int Int -> TreeTinLow Char Int Int
setTins tree = fst (setTins' tree 0)

setTins':: TreeTinLow Char Int Int -> Int -> (TreeTinLow Char Int Int, Int)
setTins' (NodeTinLow a tin low []) previous = 
    let newTin = previous + 1
    in (NodeTinLow a newTin low [], newTin)
setTins' (NodeTinLow a tin low children) previous = (NodeTinLow a newTin low children2, previous2)
    where 
        newTin = previous + 1
        (children2, previous2) = foldl (\(x, prev) y -> let (x2, prev2) = setTins' y prev in (x ++ [x2], prev2)) ([], newTin) (reverse children)

-- Sets low values to nodes as if the tree was a result of DFS traversal.
-- Assumes that tin values are already set.
setLows:: TreeTinLow Char Int Int -> Graph -> TreeTinLow Char Int Int
setLows (NodeTinLow a tin low children) g = setLows' (NodeTinLow a tin low children) g vertexTinMap a
    where
        tree = NodeTinLow a tin low children
        vertexTinMap = generateVertexTinMap tree emptyVertexValMap
        generateVertexTinMap:: TreeTinLow Char Int Int -> VertexValMap -> VertexValMap
        generateVertexTinMap (NodeTinLow a tin low children) m = 
            let m2 = Map.insert a tin m
            in foldl (\m2 x -> generateVertexTinMap x m2) m2 children


setLows':: TreeTinLow Char Int Int -> Graph -> VertexValMap -> Char -> TreeTinLow Char Int Int
setLows' (NodeTinLow a tin low []) g m parent = NodeTinLow a tin (minimum [tin, minimum [getVertexVal x m | x <- g!a ]]) []
setLows' (NodeTinLow a tin low children) g m parent = 
    let childrenNew = [setLows' x g m a | x <- children]
        low2 = minimum [tin, minimum [getVertexVal x m | x <- g!a, x/=parent ], minimum [childrenLow | (NodeTinLow _ _ childrenLow _) <- childrenNew]]
    in NodeTinLow a tin low2 childrenNew

-- Find bridges in a graph.
findBridges:: Graph -> [Edge]
findBridges g = 
    let tree = setLows (setTins (treeToTinLow (head (dfsTree g ((vertices g) !! 0))))) g
    in findBridgesT tree

-- Find bridges in a DFS tree.
findBridgesT:: TreeTinLow Char Int Int -> [Edge]
findBridgesT (NodeTinLow a tin low []) = []
findBridgesT (NodeTinLow a tin low children) = 
    let bridges = [(a, b) | (NodeTinLow b tin2 low2 _) <- children, low2 > tin]
        bridges2 = concat [findBridgesT x | x <- children]
    in bridges ++ bridges2

-- Find articulation nodes in a graph.
findArticulations::Graph -> [Char]
findArticulations g =
    let tree = setLows (setTins (treeToTinLow (head (dfsTree g ((vertices g) !! 0))))) g
    in nub (findArticulationsT tree)

-- Find articulation nodes in a tree.
findArticulationsT:: TreeTinLow Char Int Int -> [Char]
findArticulationsT (NodeTinLow a tin low []) = []
findArticulationsT (NodeTinLow a tin low children) = 
    let bridges = [a | (NodeTinLow b tin2 low2 _) <- children, low2 >= tin]
        bridges2 = concat [findArticulationsT x | x <- children]
    in bridges ++ bridges2