import System.IO
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.IntSet as IntSet

type Node = Int
type Graph = [(Node, [Node])]

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
    return parsed

getNeighbors :: Graph -> Node -> [Node]
getNeighbors graph node = fromMaybe [] (lookup node graph)

connectedComponents :: Graph -> [[Node]]
connectedComponents graph = findComponents allNodes IntSet.empty []
    where
        allNodes = map fst graph
        findComponents  [] _ components = components
        findComponents  (n:rest) visited components
            | IntSet.member n visited = findComponents  rest visited components
            | otherwise =
                let (component, visited') = dfs graph n visited
                    in findComponents rest visited' (component : components)

dfs :: Graph -> Node -> IntSet.IntSet -> ([Node], IntSet.IntSet)
dfs graph start visited = runDfs [start] visited []
    where
        runDfs [] vis acc = (reverse acc, vis)
        runDfs (n:rest) vis acc
            | IntSet.member n vis = runDfs rest vis acc
            | otherwise =
                let neighbors = getNeighbors graph n
                    vis' = IntSet.insert n vis
                in runDfs (neighbors ++ rest) vis' (n : acc)

main :: IO ()
main = do
    graph <- loadGraph "components.txt"
    print graph
    print (connectedComponents graph)