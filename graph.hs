import System.IO
import Data.Char
import Data.List
import Data.Maybe

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

main :: IO ()
main = do
    graph <- loadGraph "example.txt"
    print graph