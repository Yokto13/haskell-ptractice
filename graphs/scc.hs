-- Graph represented as adjacency list
import Control.Monad.ST
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

type Table a = Array Char a
type Graph = Table [Char]
type Edge = (Char, Char)
type Bounds = (Char, Char)
type Forest a = [Tree a]

data Tree a = Node a (Forest a) deriving (Show)

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

{-
removeVertex:: Graph -> Char -> [Graph]
removeVertex g v = buildComponents (removeVertex' g v)
    where
        removeVertex' :: Graph -> Char -> Graph
        removeVertex' g v = buildGraph (bounds g) [(a, b) | (a, b) <- edges g, not (a==v), not(a==b)]
        buildComponents :: Graph -> [Graph]
        buildComponents g = 
            let components = dfsAll g [u | u <- vertices g, u /= v] emptyVertexSet []
            in [buildGraph (bounds component)]
            -}

{-
dfsComponents :: Graph -> [Char] -> VertexSet -> [Graph]
dfsComponents g [] _ = []
dfsComponents g (v:vs) vertexSet
    | isVertexVisited v vertexSet = dfsComponents g vs vertexSet
    | otherwise =
        let 
            reachable = dfs g v emptyVertexSet
            -}

        

-- postOrderF :: Tree a -> [a]
-- postOrderF (Node a f) = (concat [postOrder t | t <- f]) ++ a

-- postOrder :: Forest a -> [a]
-- postOrder g = postOrderF . dfsF g

-- topoSort :: Graph -> [Char]
-- topoSort g = reverse (postOrder (dfsF g))


--dfsF :: Graph -> Forest Char
--dfsF g = dfs g (vertices g)

{-}
generate:: Graph -> Char -> VertexSet -> Tree Char
generate g v vs = Node v children
    where 
        ns = addVisitedVertex v vs
        children = dfsns (g!v) ns
        dfsns :: String -> VertexSet -> Forest Char
        dfsns [] _ = []
        dfsns (x:xs) ns = 
            if isVertexVisited x ns 
            then
                []
            else
                generate g x nss : dfsns xs nss
                where
                    nss = addVisitedVertex x ns

inner_dfs :: Graph -> [Char] -> VertexSet -> (Forest Char, VertexSet)
inner_dfs g [] visited = ([], visited)
inner_dfs g (x:xs) visited = 
    if isVertexVisited x visited
    then
        inner_dfs g xs visited
    else
        let (forest, new_visited) = dfs3 g x visited
        in (generate g x new_visited : forest, new_visited)

dfs3 :: Graph -> Char -> VertexSet -> (Forest Char, VertexSet)
dfs3 g v visited = 
    if isVertexVisited v visited
    then
        ([], visited)
    else
        Node v (inner_dfs g (g!v) visited)
    where
        new_visited = addVisitedVertex v visited
        -}


-- Define VertexSet and methods to work with it
type VertexSet = Set.Set Char

emptyVertexSet :: VertexSet
emptyVertexSet = Set.empty

isVertexVisited :: Char -> VertexSet -> Bool
isVertexVisited v vertexSet = Set.member v vertexSet

addVisitedVertex :: Char -> VertexSet -> VertexSet
addVisitedVertex vertex vertexSet = Set.insert vertex vertexSet

type SubtreeSizeMap = Map.Map Char Int

emptySubtreeSizeMap :: SubtreeSizeMap
emptySubtreeSizeMap = Map.empty

addSubtreeSize :: Char -> Int -> SubtreeSizeMap -> SubtreeSizeMap
addSubtreeSize vertex size subtreeSizeMap = Map.insert vertex size subtreeSizeMap

getSubtreeSize :: Char -> SubtreeSizeMap -> Int
getSubtreeSize vertex subtreeSizeMap = subtreeSizeMap Map.! vertex

degree:: Graph -> Char -> Int
degree g v = length (g!v)

calculateSubtreeSizes :: Graph -> Char -> SubtreeSizeMap -> Char -> SubtreeSizeMap
calculateSubtreeSizes graph parent subtreeMap root
    | parent /= root && degree graph root == 1 = addSubtreeSize root 1 subtreeMap
    | otherwise = 
        let children = [ch | ch <- graph!root, ch /= parent]
            subtreeMap2 = foldl (calculateSubtreeSizes graph root) subtreeMap children
            subtreeSizes = map (\x -> getSubtreeSize x subtreeMap2) children
            subtreeSize = 1 + (sum subtreeSizes)
        in addSubtreeSize root subtreeSize subtreeMap2

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


findCentroid :: Graph -> Char -> Char -> Int -> SubtreeSizeMap -> (Char, String)
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
                    neighborsSizePairs = [(x, getSubtreeSize x subtreeMap) | x <- neighbors2]
                in foldl (\(x, sz) (u, max_sz) -> if sz > max_sz then (x, sz) else (u, max_sz)) (' ', 0) neighborsSizePairs

centroidDecomposition :: Graph -> Char -> Tree Char
centroidDecomposition g v = 
    let subtreeMap = calculateSubtreeSizes g v emptySubtreeSizeMap v
        (centroid, neighbors) = findCentroid g v v (getSubtreeSize v subtreeMap) subtreeMap
        splitted_g = removeVertex g centroid
    in if neighbors == []
    then
        Node centroid []
    else
        --trace ("Centroid: " ++ [centroid] ++ " Neighbors: " ++ neighbors)
        Node centroid [centroidDecomposition splitted_g x | x <- neighbors]