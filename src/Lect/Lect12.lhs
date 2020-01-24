% CS 340: Programming Paradigms and Patterns
% Lect 12 - Search
% Michael Lee

> module Lect.Lect12 where
> import Data.List
> import Data.Maybe
> import System.IO
> import Debug.Trace

Search
======

The search problem is one of the most common problems you'll encounter across
various domains of computer science. Searching amounts to the problem of
looking for information that matches some criteria in a collection of data.
Different search algorithms and problem domains specify the way the data is
collected, represented, and stored, and what the search criteria are.

A reasonably general start to our discussion of search is to consider
how we might search for a value stored somewhere in an N-way tree.

Here are some definitions we previously came up with for generating simple
binary trees:

> data Tree a = Node {
>   rootVal :: a,
>   forest :: [Tree a]
> } deriving (Show)
> 
> instance Functor Tree where
>   fmap f (Node x ts) = Node (f x) $ fmap (fmap f) ts

> binTree :: Tree Integer
> binTree = t 1
>   where t n = Node n [t (2*n), t (2*n+1)]
>
> pruneTree :: Int -> Tree a -> Tree a
> pruneTree 1 (Node x _) = Node x []
> pruneTree n (Node x f) = Node x $ map (pruneTree (n-1)) f

What would our search function look like?

> treeSearch :: (a -> Bool) -> ([Tree a] -> [Tree a] -> [Tree a])
>   -> [Tree a] -> Bool
>
> treeSearch goal combiner [] = False
> treeSearch goal combiner ((Node x forest):ns)
>   | goal x = True
>   | null forest = treeSearch goal combiner ns
>   | otherwise = treeSearch goal combiner (combiner forest ns)

> depthFirstSearch :: (a -> Bool) -> Tree a -> Bool
> depthFirstSearch goal t = treeSearch goal (++) [t]

> breadthFirstSearch :: (a -> Bool) -> Tree a -> Bool
> breadthFirstSearch goal t = treeSearch goal (flip (++)) [t]

> data Maze = Maze Int Int String deriving Show
> type MazeLoc = (Int,Int)
>
> maze1 = Maze 4 3 "####\
>                  \    \
>                  \####" -- exit (3,1)
>
> maze2 = Maze 5 6 "#####\
>                  \    #\
>                  \# # #\
>                  \# # #\
>                  \# #  \
>                  \#####" -- exit (4,4)
> 
> maze3 = Maze 17 7 "#################\
>                   \  #       #     #\
>                   \# ##### # # # # #\
>                   \#     # # # # # #\
>                   \# ### ### # # ###\
>                   \#   #       #    \
>                   \#################" -- exit (16,5)
>
> maze4 = Maze 17 7 "#################\
>                   \                #\
>                   \# # # # # # # # #\
>                   \# # # # # # # # #\
>                   \# ###############\
>                   \#                \
>                   \#################" -- exit (16,5)
>
> maze5 = Maze 16 9 "################\
>                   \               #\
>                   \### ########## #\
>                   \#   #          #\
>                   \# ### ##########\
>                   \# ###          #\
>                   \# ############ #\
>                   \#               \
>                   \################" -- exit (15,7)
>
> mazeChar :: Maze -> MazeLoc -> Char
> mazeChar (Maze w h s) (x,y) = s !! (w*y + x)
>
> mazeMoves :: Maze -> MazeLoc -> [(Int,Int)]
> mazeMoves m@(Maze w h s) (x,y)
>   | mazeChar m (x,y) == ' ' = [(x+dx,y+dy)
>                               | dx <- [-1,0,1], dy <- [-1,0,1],
>                                 (dx == 0 && dy /= 0) || (dx /= 0 && dy == 0),
>                                 x+dx >= 0 && x+dx < w,
>                                 y+dy >= 0 && y+dy < h,
>                                 mazeChar m ((x+dx),(y+dy)) == ' ']
>   | otherwise = []
>
> mazeVisit :: Maze -> MazeLoc -> Maze
> mazeVisit m@(Maze w h s) (x,y) = let offset = w*y + x
>                                  in Maze w h $ (take offset s)
>                                     ++ "X"
>                                     ++ (drop (offset+1) s)
> 
> search :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> ([a] -> [a] -> [a])
>   -> [a] -> [a] -> Maybe a
> search goal succ comb nodes oldNodes
>   | null nodes = Nothing
>   | goal (head nodes) = Just (head nodes)
>   | otherwise = let (n:ns) = nodes
>                 in traceShow (n,nodes) $ search goal succ comb
>                    (comb (removeDups (succ n)) ns)
>                    (adjoin n oldNodes)
>   where removeDups = filter (not . ((flip elem) (nodes ++ oldNodes)))
>         adjoin x lst = if elem x lst then lst else x:lst
>
> dfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
> dfs goal succ start = search goal succ (++) [start] []
>
> bfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
> bfs goal succ start = search goal succ (flip (++)) [start] []

> mazeSearch :: Maze -> MazeLoc -> MazeLoc -> Bool
> mazeSearch m inloc outloc =
>   case dfs (== outloc) (mazeMoves m) inloc
>   -- case bfs (== outloc) (mazeMoves m) inloc
>   -- case bestFirstSearch (== outloc) (mazeMoves m) (mazeDist outloc) inloc
>   of Nothing -> False
>      otherwise -> True

> bestFirstSearch :: (Eq a, Show a, Ord b) => (a -> Bool) -> (a -> [a])
>                 -> (a -> b) -> a -> Maybe a
> bestFirstSearch goal succ score start = search goal succ comb [start] []
>   where comb new old = sortOn score (new ++ old)

> mazeDist :: MazeLoc -> MazeLoc -> Double
> mazeDist (x1,y1) (x2,y2) = sqrt $ fromIntegral $ (x1-x2)^2 + (y1-y2)^2

> data MazePath = MazePath [MazeLoc] deriving Eq
>
> instance Show MazePath where
>   show (MazePath p) = show (length p) ++ "<" ++ show (reverse p) ++ ">"
>
> mazePathSearch :: Maze -> MazeLoc -> MazeLoc -> Maybe MazePath
> mazePathSearch m inloc outloc =
>   bestFirstSearch (\(MazePath (l:_)) -> l == outloc)
>                   nextPaths scorePath $ MazePath [inloc]
>   where nextPaths (MazePath p@(l:_)) = map (\nl -> MazePath $ nl:p)
>                                        $ filter (not . (flip elem) p)
>                                        $ mazeMoves m l
>         scorePath (MazePath p@(l:_)) = mazeDist l outloc
>                                        + fromIntegral (length p)

Minimax
-------

> data Piece = X | O deriving (Eq, Show, Read)
> data Board = Board {
>   squares :: [Maybe Piece]
>   } deriving (Eq)
> 
> instance Show (Board) where
>   show (Board ps) = intercalate "\n"
>                     $ map (foldr1 (++))
>                     $ chunksOf 3
>                     $ map showSquare ps
>     where showSquare Nothing = "~"
>           showSquare (Just p) = show p
> 
> emptyBoard :: Board
> emptyBoard = Board $ replicate 9 Nothing
> 
> readBoard :: String -> Board
> readBoard = Board . (map f)
>   where f '~' = Nothing
>         f p = Just $ read [p]
> 
> -- e.g., readBoard "O~~~X~~~~"        
> 
> chunksOf :: Int -> [a] -> [[a]]
> chunksOf _ [] = []
> chunksOf n l = take n l : chunksOf n (drop n l)
> 
> -- e.g., chunksOf 3 "123456789"
> 
> placePiece :: Int -> Piece -> Board -> Board
> placePiece n p b = let (pre,_:post) = splitAt n $ squares b
>                    in Board $ pre ++ [Just p] ++ post
> 
> -- e.g., placePiece 1 O $ placePiece 0 X emptyBoard
> 
> allEqual :: (Eq a) => [a] -> Bool
> allEqual [] = True
> allEqual (x:xs) = all (== x) xs
>                 
> winner :: Board -> Maybe Piece
> winner b = let sqs = squares b
>                rows = filter (all (/= Nothing)) $ chunksOf 3 sqs
>                cols = filter (all (/= Nothing)) $ transpose $ chunksOf 3 sqs
>                diags = filter (all (/= Nothing))
>                        $ map (map (sqs !!)) [[0,4,8],[2,4,6]]
>                winning = find allEqual $ rows ++ cols ++ diags
>            in case winning of Nothing -> Nothing
>                               Just (p:ps) -> p
> 
> won :: Board -> Bool
> won b = not $ isNothing $ winner b
> 
> drawn :: Board -> Bool
> drawn b = all (/= Nothing) (squares b) && not (won b)
> 
> opponent :: Piece -> Piece
> opponent O = X
> opponent X = O
> 
> -- building up the idea of analyzing board position to compute a numeric score
> 
> data Scored a = Scored Int a deriving (Show)
> 
> instance Eq (Scored a) where
>   (Scored x _) == (Scored y _) = x == y
> 
> instance Ord (Scored a) where
>   compare (Scored x _) (Scored y _) = compare x y
> 
> fromScored :: Scored a -> a
> fromScored (Scored _ x) = x
> 
> scoredBoard :: Piece -> Board -> Scored Board
> scoredBoard p b
>   | winner b == Just p = Scored 100 b
>   | winner b == Just (opponent p) = Scored (-100) b
>   | drawn b = Scored 0 b
>   | otherwise = Scored waysToWin b
>   where waysToWin = let opp = opponent p
>                         sqs = squares b
>                         rows = filter (all (/= Just opp)) $ chunksOf 3 sqs
>                         cols = filter (all (/= Just opp))
>                                $ transpose $ chunksOf 3 sqs
>                         diags = filter (all (/= Just opp))
>                                 $ map (map (sqs !!)) [[0,4,8],[2,4,6]]
>                     in length $ filter (any (== Just p)) $ rows ++ cols ++ diags
>     
> turn :: Board -> Piece
> turn b = let xs = length $ filter (== Just X) $ squares b
>              os = length $ filter (== Just O) $ squares b
>          in if xs <= os then X else O
> 
> boardSuccs :: Board -> [Board]
> boardSuccs b
>   | won b || drawn b = []
>   | otherwise = let t = turn b
>                 in map (\i -> placePiece i t b)
>                    $ findIndices isNothing $ squares b
> 
> -- e.g., boardSuccs $ readBoard "~~~~X~~~~"
> -- e.g., boardSuccs $ readBoard "O~~~X~~~~"
> -- (may want to first remove newlines from show fn before the next example.)
> -- e.g., map (scoredBoard X) $ boardSuccs $ readBoard "OOX~X~~~~" 
> -- e.g., maximumBy (comparing $ scoredBoard X)
> --                 $ boardSuccs $ readBoard "OOX~X~~~~"
> 
> nextBestBoard :: Board -> Maybe Board
> nextBestBoard b
>   | null succs = Nothing
>   | otherwise = Just $ fromScored $ maximum $ map (scoredBoard (turn b)) succs
>   where succs = boardSuccs b
> 
> autoplay :: [Board]
> autoplay = emptyBoard : next autoplay
>   where next (b:bs) = let mb = nextBestBoard b
>                       in case mb of Nothing -> []
>                                     Just b' -> b' : next bs
> 
> -- note: note the best play! (only immediate wins are taken automatically --
> --       good/bad plays further down the road do not inform play)
> 
> playGame :: (Board -> Maybe Board) -> Board -> IO ()
> playGame strategy b
>   | won b = putStrLn $ show (opponent t) ++ " won!"
>   | drawn b = putStrLn "Draw game!"
>   | t == X = do
>       putStr $ "Enter move for X: "
>       hFlush stdout
>       i <- readLn
>       if isNothing $ (squares b) !! i
>         then do
>           let b' = placePiece i t b
>           print b'
>           putStrLn ""
>           playGame strategy b'
>         else do
>           putStrLn "Invalid move"
>           playGame strategy b
>   | otherwise = do
>       let Just b' = strategy b
>       putStrLn "Computer moves:"
>       print b'
>       putStrLn ""
>       playGame strategy b'
>   where t = turn b

