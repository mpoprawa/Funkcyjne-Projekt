import System.IO
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap

type Node = Int
type Graph = IntMap.IntMap [Node]

parseLine :: String -> Maybe (Node, [Node])
parseLine line =
    case break (== ':') line of
        (nodeString, ':' : rest) ->
            let node = read nodeString :: Int
                edges = parseEdges rest
            in Just (node, edges)
        _ -> Nothing

parseEdges :: String -> [Node]
parseEdges edges = mapMaybe parseEdge (splitBy ',' edges)

parseEdge :: String -> Maybe Node
parseEdge []    = Nothing
parseEdge edge  = Just (read edge)

splitBy :: Char -> String -> [String]
splitBy delim str =
    let (s1, s2) = break (== delim) str
    in s1 : case s2 of
        []          -> []
        (delim:xs)  -> splitBy delim xs

loadGraph :: FilePath -> IO Graph
loadGraph path = do
    file <- readFile path
    let fileLines = lines file
        parsed = mapMaybe parseLine fileLines
    return (IntMap.fromList parsed)

getNeighbors :: Graph -> Node -> [Node]
getNeighbors graph node = IntMap.findWithDefault [] node graph

connectedComponents :: Graph -> [[Node]]
connectedComponents graph = findComponents (IntMap.keys graph) IntSet.empty []
    where
        findComponents  [] _ components = components
        findComponents  (n:rest) visited components
            | IntSet.member n visited = findComponents  rest visited components
            | otherwise =
                let (component, visited') = dfs graph n visited
                    in findComponents rest visited' (component : components)

dfs :: Graph -> Node -> IntSet.IntSet -> ([Node], IntSet.IntSet)
dfs graph start visited = runDfs [start] visited []
    where
        runDfs [] vis acc = (acc, vis)
        runDfs (n:rest) vis acc
            | IntSet.member n vis = runDfs rest vis acc
            | otherwise =
                let neighbors = getNeighbors graph n
                    vis' = IntSet.insert n vis
                in runDfs (neighbors ++ rest) vis' (acc ++ [n])

bfs :: Graph -> Node -> Int
bfs graph start = runBfs [(start, 0)] IntSet.empty 0
    where
        runBfs [] _ maxDist = maxDist
        runBfs ((n, d):queue) visited maxDist
            | IntSet.member n visited = runBfs queue visited maxDist
            | otherwise =
                let visited' = IntSet.insert n visited
                    neighbors = getNeighbors graph n
                    newQueue = queue ++ [(nei, d + 1) | nei <- neighbors, not (IntSet.member nei visited')]
                    maxDist' = max maxDist d
                in runBfs newQueue visited' maxDist'

componentDiameter :: Graph -> [Node] -> Int
componentDiameter graph [] = 0
componentDiameter graph component = maximum (map (bfs graph) component)

maxDiameter :: Graph -> Int
maxDiameter graph =
    let comps = connectedComponents graph
    in maximum (map (componentDiameter graph) comps)

main :: IO ()
main = do
    graph <- loadGraph "graphs/components.txt"
    --print graph
    print (connectedComponents graph)
    print (maxDiameter graph)