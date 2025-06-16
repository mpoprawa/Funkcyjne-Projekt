import System.IO
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..))

type Node = Int
--type Graph = IntMap.IntMap [Node]
type Graph = V.Vector [Node]

newtype RawString = RawString String

instance Show RawString where
    show (RawString s) = s

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

collectEdges :: [(Node, [Node])] -> IntMap.IntMap [Node]
collectEdges = IntMap.fromListWith (++)

loadGraph :: FilePath -> IO Graph
loadGraph path = do
    file <- readFile path
    let fileLines = lines file
        parsed = mapMaybe parseLine fileLines
        maxNode = if null parsed then 0 else maximum (map fst parsed)
        edgesByNode = collectEdges parsed
        lookupNeighbors i = IntMap.findWithDefault [] i edgesByNode
        graphVec = V.generate (maxNode + 1) lookupNeighbors
    return graphVec
    --return (IntMap.fromList parsed)

getNeighbors :: Graph -> Node -> [Node]
getNeighbors graph node = graph V.! node
--getNeighbors graph node = IntMap.findWithDefault [] node graph

connectedComponents :: Graph -> [[Node]]
--connectedComponents graph = findComponents (IntMap.keys graph) IntSet.empty []
connectedComponents graph = findComponents [0 .. V.length graph - 1] IntSet.empty []
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
--bfs graph start = runBfs (Seq.singleton (start, 0)) IntSet.empty 0
bfs graph start = runBfs [(start, 0)] IntSet.empty 0
    where
        --runBfs [] _ maxDist = maxDist
        runBfs Seq.Empty _ maxDist = maxDist
        --runBfs ((n, d):queue) visited maxDist
        runBfs ((n, d) Seq.:<| queue) visited maxDist
            | IntSet.member n visited = runBfs queue visited maxDist
            | otherwise =
                let visited' = IntSet.insert n visited
                    neighbors = getNeighbors graph n
                    newQueue = queue Seq.>< Seq.fromList [(nei, d + 1) | nei <- neighbors, not (IntSet.member nei visited')]
                    --newQueue = queue ++ [(nei, d + 1) | nei <- neighbors, not (IntSet.member nei visited')]
                    maxDist' = max maxDist d
                in runBfs newQueue visited' maxDist'

componentDiameter :: Graph -> [Node] -> Int
componentDiameter graph [] = 0
componentDiameter graph component = maximum (map (bfs graph) component)

maxDiameter :: Graph -> Int
maxDiameter graph =
    let comps = connectedComponents graph
    in maximum (map (componentDiameter graph) comps)

degrees :: Graph -> [(Node, Int)]
degrees graph = [(node, length (getNeighbors graph node)) | node <- [0 .. V.length graph - 1]]
--degrees graph = [(node, length (getNeighbors graph node)) | node <- IntMap.keys graph]

--minDistances :: Graph -> Node -> [(Node, Maybe Int)]
--minDistances graph node =
--    let bfsDistances [] _ acc = acc
--        bfsDistances ((n, d):queue) visited acc
--            | IntSet.member n visited = bfsDistances queue visited acc
--            | otherwise =
--                let visited' = IntSet.insert n visited
--                    neighbors = getNeighbors graph n
--                    newQueue = queue ++ [(nei, d + 1) | nei <- neighbors, not (IntSet.member nei visited')]
--                    acc' = IntMap.insert n d acc
--                in bfsDistances newQueue visited' acc'
--        distancesMap = bfsDistances [(node, 0)] IntSet.empty IntMap.empty
--        allNodes = IntMap.keys graph
--    in [(n, IntMap.lookup n distancesMap) | n <- allNodes]

minDistances :: Graph -> Node -> [(Node, Maybe Int)]
minDistances graph node =
    let n = V.length graph
        bfsDistances Seq.Empty visited acc = acc
        --bfsDistances [] _ acc = acc
        --bfsDistances ((v, d):queue) visited acc
        bfsDistances ((v, d) :<| queue) visited acc
            | IntSet.member v visited = bfsDistances queue visited acc
            | otherwise =
                let visited' = IntSet.insert v visited
                    neighbors = getNeighbors graph v
                    newQueue = queue Seq.>< Seq.fromList [(nei, d + 1) | nei <- neighbors, not (IntSet.member nei visited')]
                    --newQueue = queue ++ [(nei, d + 1) | nei <- neighbors, not (IntSet.member nei visited')]
                    acc' = acc V.// [(v, Just d)]
                in bfsDistances newQueue visited' acc'
        --distancesMap = bfsDistances [(node, 0)] IntSet.empty (V.replicate n Nothing)
        distancesMap = bfsDistances (Seq.singleton (node, 0)) IntSet.empty (V.replicate n Nothing)
    in [(i, distancesMap V.! i) | i <- [0 .. n - 1]]

graphDistances :: Graph -> [((Node, Node), Maybe Int)]
graphDistances graph =
    --let nodes = IntMap.keys graph
    let nodes = [0 .. V.length graph - 1]
    in [((n1, n2), d) | n1 <- nodes, let distances = minDistances graph n1, (n2, d) <- distances]

--clasterizationCoefficients :: Graph -> IntMap.IntMap [Double]
clasterizationCoefficients :: Graph -> V.Vector [Double]
clasterizationCoefficients graph =
    V.imap clustCoeff graph
    --IntMap.mapWithKey clustCoeff graph
    where
        clustCoeff node neighbors
            | length neighbors < 2 = [0.0]
            | otherwise =
                let neighborPairs = [(x, y) | (x:ys) <- tails neighbors, y <- ys]
                    links = [1 | (x, y) <- neighborPairs, y `elem` getNeighbors graph x]
                    k = length neighbors
                    coeff = (fromIntegral (sum links)) / (fromIntegral (k * (k - 1)) / 2)
                in [coeff]

maxDegree :: Graph -> Int
maxDegree graph = maximum (map snd (degrees graph))

minDegree :: Graph -> Int
minDegree graph = minimum (map snd (degrees graph))

avgDegree :: Graph -> Double
avgDegree graph =
    let degs = map snd (degrees graph)
        count = fromIntegral (length degs)
    in if count == 0 then 0 else fromIntegral (sum degs) / count

avgDistance :: Graph -> Double
avgDistance graph =
    let dists = [fromIntegral d | ((n1, n2), Just d) <- graphDistances graph, n1 /= n2]
        count = fromIntegral (length dists)
    in if count == 0 then 0 else sum dists / count

avgClustCoeff :: Graph -> Double
avgClustCoeff graph =
    let coeffs = concat (V.toList (clasterizationCoefficients graph))
    --let coeffs = concat (IntMap.elems (clasterizationCoefficients graph))
        count = fromIntegral (length coeffs)
    in if count == 0 then 0 else sum coeffs / count

printGraphDistances :: Graph -> RawString
printGraphDistances graph = 
    RawString (unlines (map format (graphDistances graph)))
    where
        format ((from, to), maybeDist) =
            show from ++ " to " ++ show to ++ ": " ++
            case maybeDist of
                Just n  -> show n
                Nothing -> "unreachable"

run :: Show a => (Graph -> a) -> IO ()
run function = do
    graph <- loadGraph "graphs/medium.txt"
    print (function graph)

main :: IO ()
main = do
    graph <- loadGraph "graphs/small.txt"
    --print graph
    print (connectedComponents graph)
    print (maxDiameter graph)
    print (minDegree graph)
    print (maxDegree graph)
    print (avgDegree graph)
    mapM_ print (graphDistances graph)
    print (avgDistance graph)
    mapM_ print (clasterizationCoefficients graph)
    print (avgClustCoeff graph)