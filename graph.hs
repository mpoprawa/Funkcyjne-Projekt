import System.IO
import Data.Char
import Data.List
import Data.Maybe

type Node = Int
type Weight = Int
type Edge = (Node, Weight)
type Graph = [(Node, [Edge])]

parseLine :: String -> Maybe (Node, [Edge])
parseLine line =
    case break (== ':') line of
        (nodeString, ':' : rest) ->
            let node = read nodeString :: Int
                edges = parseEdges rest
            in Just (node, edges)
        _ -> Nothing

parseEdges :: String -> [Edge]
parseEdges edges = mapMaybe parseEdge (splitBy ',' edges)

parseEdge :: String -> Maybe Edge
parseEdge edge = 
    case words edge of
        [node, weight]  -> Just (read node, read weight)
        _               -> Nothing

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy delim str =
    let (s1, s2) = break (== delim) str
    in s1 : case s2 of
        []          -> []
        (delim:xs)  -> splitBy delim xs

splitBy2 :: Char -> String -> [String]
splitBy2 delim = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == delim = [] : acc
      | otherwise  = (c : x) : xs

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