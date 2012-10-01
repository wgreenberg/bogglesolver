import System.Environment
import Text.Read
import Data.Graph
import Data.Ord 
import Data.List
import Data.ListTrie.Base.Map (Map,AList, WrappedIntMap)
import Data.ListTrie.Set (TrieSet)
import qualified Data.ListTrie.Set as T

main = do
	dict <- readFile "dict.txt" 
	problem <- readFile "problem.txt" 
	return (solve (lines problem) (lines dict))

solve :: [String] -> [String] -> [String]
solve problem dict = 
    let dictionaryTrie = fromListA dict
        letterForest = map (treeify problem) [1..16]
        prunedForest = map (prune dict problem) letterForest 
        allWords = nub [word | word <- concat $ map (wordFromRoot problem) prunedForest, T.member word dictionaryTrie]
    in reverse $ sortBy (comparing length) allWords

{-- Trie stuff --}

fromListA :: Eq a => [[a]] -> TrieSet AList a
fromListA = T.fromList

{-- Tree stuff --}

data STree = Tip | STree { board :: Graph,
                           letter :: Char,
                           key :: Int } deriving (Show, Eq)

treeify :: [String] -> Int -> STree
treeify problem keyz = STree {board = problemGraph, letter = (keyToLetter problem keyz), key = keyz}

prune :: [String] -> [String] -> STree -> STree
prune dict problem root =
    let dictTrie = fromListA dict
        pruneKeys = 
        	foldr (\e i -> if nonwordByPrefix2 [letter root, letter e] then (key e):i else i) [] ((children problem root) \\ [Tip])
        edgesToPrune = map (\i -> ((key root), i)) pruneKeys
        prunedGraph = removeEdges (board root) edgesToPrune
    in STree {board = prunedGraph, letter = (letter root), key = (key root)}

wordFromRoot :: [String] -> STree -> [String]
wordFromRoot problem Tip = [""]
wordFromRoot problem node = map (\e -> [(letter node)] ++ e) (concat (map (wordFromRoot problem) (children problem node)))

children :: [String] -> STree -> [STree]
children problem Tip = []
children problem node = 
	let newGraph = partialGraph (board node) [key node]
	in foldr (\e i -> STree {board = newGraph, letter = (keyToLetter problem e), key = e}:i) [Tip] (neighborsByKey (key node) (board node))

keyToLetter :: [String] -> Int -> Char
keyToLetter problem key = concat problem !! (key - 1)

{-- Graph stuff --}

-- Return the basic neighbor graph of a 4x4 grid
problemGraph :: Graph
problemGraph = buildG (1,16) (bidirect . neighborPairs $ numberBoard)

emptyGraph :: Graph
emptyGraph = buildG (1,16) []

removeEdges :: Graph -> [(Int, Int)] -> Graph
removeEdges graph remEdges = buildG (1,16) (filter (flip notElem (bidirect remEdges)) (edges graph))

-- Takes a graph and a list of keys to omit, returns a graph without edges to/from those keys
partialGraph :: Graph -> [Int] -> Graph
 --partialGraph omitKeys = 
	--buildG (1,16) [edge | edge <- (bidirect . neighborPairs $ numberBoard), not (edgeHasKeys omitKeys edge)]
partialGraph graph omitKeys = buildG (1,16) (filter (not . edgeHasKeys omitKeys) (edges graph))

numberBoard = [[1..4],[5..8],[9..12],[13..16]]

diagRelations :: [[Int]]
diagRelations = [[1,6,11,16],[9,14],[5,10,15],[2,7,12],[3,8],[2,5],[3,6,9],[4,7,10,13],[8,11,14],[12,15]]

easyBoard = [[1,2,3],[4,5,6],[7,8,9]]

-- Takes a list of elements and returns list of every consecutive sublist of length 2 (e.g. [1,2,3] -> [[1,2],[2,3]])
pairify :: Eq a => [a] -> [[a]]
pairify ls = [[e1,e2] | e1 <- ls, e2 <- ls, [e1,e2] `isInfixOf` ls]

-- Takes a list of sublists (of length 2) and returns a list of tuples
tuplify :: [[a]] -> [(a, a)]
tuplify = foldr (\e i -> (e !! 0, e !! 1):i) []

-- Takes a NxN matrix and returns a list of tuples which indicate the 'neighbor' relationship
neighborPairs :: [[Int]] -> [(Int,Int)]
neighborPairs problem = 
	let horizTuples = problem >>= tuplify . pairify
   	    vertTuples = transpose problem >>= tuplify . pairify
   	    diagTuples = diagRelations >>= tuplify . pairify
    in horizTuples ++ vertTuples ++ diagTuples

-- Takes a scramble problem and attaches a key to each letter
keyify :: [String] -> [(Char, Int)]
keyify problem = zip (concat problem) [1..]

-- Given a list of edges, return the transpose, e.g. (a,b) -> (b,a)
transposeEdges :: [(a,a)] -> [(a,a)]
transposeEdges = foldr (\e i -> (snd e, fst e):i) []

-- Given a list of edges, return the same list concatted with the transpose of the list
bidirect :: [(a,a)] -> [(a,a)]
bidirect edges = edges ++ (transposeEdges edges)

-- Given a list of edges, remove all edges which are the transpose of another edge
undirect :: [(Int, Int)] -> [(Int, Int)]
undirect = foldl (\i e -> if (snd e, fst e) `notElem` i then e:i else i) []

-- Edges stored as (Int, Int)
neighborsByKey :: Int -> Graph -> [Int]
neighborsByKey key graph = [neighbor | (home, neighbor) <- filter (edgeHasKeys [key]) (edges graph), home == key]

isEdge :: (Int, Int) -> (Int, Int) -> Bool
isEdge edge1 edge2 = elem (fst edge1) [(fst edge2), (snd edge2)] && elem (snd edge1) [(fst edge2), (snd edge2)]

-- Returns true if either vertex on edge is one of the keys
edgeHasKeys :: [Int] -> (Int, Int) -> Bool
edgeHasKeys keys edge = elem (fst edge) keys || elem (snd edge) keys

nonwords = ["bb","bc","bf","bg","bj","bk","bm","bn","bp","bq","bs","bt","bv","bx","bz","cb","cc","cd","cf","cg","cj","ck","cm","cn","cp","cq","cs","cv","cx","db","dc","dd","df","dg","dk","dl","dm","dn","dp","dq","ds","dt","dv","dx","dz","ez","fb","fc","fd","ff","fg","fh","fk","fm","fn","fp","fq","fs","ft","fv","fw","fx","fz","gb","gc","gd","gf","gg","gk","gm","gp","gq","gs","gt","gv","gx","gz","hb","hc","hd","hf","hg","hh","hj","hk","hl","hn","hp","hq","hr","hs","ht","hv","hx","hz","ie","ii","ij","iq","iu","iy","jb","jc","jd","jf","jg","jh","jj","jk","jl","jm","jp","jq","jr","js","jt","jv","jw","jx","jy","jz","kc","kd","kf","kg","kj","kk","km","kp","kq","ks","kt","kx","kz","lb","lc","ld","lf","lg","lh","lj","lk","lm","ln","lp","lq","lr","ls","lt","lv","lx","lz","mc","md","mf","mg","mj","mk","ml","mp","mq","ms","mt","mv","mw","mx","mz","nb","nc","nd","nf","nh","nj","nk","nl","nm","nn","np","nq","nr","ns","nv","nw","nx","nz","oj","pb","pc","pd","pg","pj","pk","pm","pp","pq","pv","pw","px","pz","qb","qc","qd","qe","qf","qg","qh","qj","qk","ql","qm","qn","qp","qq","qr","qs","qt","qv","qx","qy","qz","rb","rc","rd","rf","rg","rj","rk","rl","rm","rn","rp","rq","rr","rs","rt","rv","rw","rx","rz","sb","sd","sg","ss","sx","sz","tb","tc","td","tf","tg","tj","tk","tl","tn","tp","tq","tt","tv","tx","ua","uc","ue","uj","uo","uq","uu","uw","uy","uz","vb","vc","vd","vf","vg","vh","vj","vk","vl","vm","vn","vp","vq","vs","vt","vv","vw","vx","vz","wb","wc","wd","wf","wg","wj","wk","wl","wm","wn","wp","wq","ws","wt","wv","ww","wx","wz","xb","xc","xd","xf","xg","xh","xj","xk","xl","xm","xn","xo","xp","xq","xr","xs","xt","xv","xw","xx","xz","yb","yd","yf","yg","yh","yj","yk","ym","yn","yq","yr","ys","yv","yx","yy","yz","zb","zc","zd","zf","zg","zh","zj","zk","zm","zn","zp","zq","zr","zs","zt","zv","zx","zz"]

nonwordByPrefix2 :: String -> Bool
nonwordByPrefix2 word = elem (take 2 word) nonwords