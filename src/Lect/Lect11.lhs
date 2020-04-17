% CS 340: Programming Paradigms and Patterns
% Lect 11 - Search
% Michael Lee

\begin{code}
-- note that we use the \begin{code} and \end{code} markers in this literate
-- source file instead of '>' line prefixes --- this is because there's going 
-- to be a bit more code than usual!

module Lect.Lect11 where
import Data.List
import Data.List.Split (chunksOf)
import Data.Map (Map, empty, fromList, update, adjust, 
                 findWithDefault, member, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Monad.State
import Debug.Trace
\end{code}


Search
======

Agenda:
  - Why and How?
  - Maze-building
    - Random values and State
  - Search
    - Uninformed search
    - Informed search
  - Adversarial search


Why and How?
------------

Search is one of the most common programming tasks you'll perform. It's
useful to recognize some common search patterns and techniques, and understand
how they might be applied.

We'll explore a few problems that are natural candidates for search-based
solutions, starting with maze-solving. As a fun programming exercise, we'll
start by considering how to generate random mazes of arbitrary size.


Maze-building
-------------

We need some data types to represent mazes. Note that we will be using the `Map`
type defined in `Data.Map` to help us keep track of connected cells within a maze.

\begin{code}
type MazeLoc = (Int, Int) -- (x,y); (1,1) @ top-left

type MazeDims = (Int, Int) -- (width,height)

data Maze = Maze { 
              mazeDims :: MazeDims, 
              mazeCells :: Map MazeLoc [MazeLoc] 
            } deriving (Show, Eq)
\end{code}


Next, some utility functions for building up our mazes:

\begin{code}
adjLocs :: MazeDims -> MazeLoc -> [MazeLoc]
adjLocs (w, h) (x, y) = 
  [(x', y') | (dx, dy) <- [(-1,0), (0,-1), (1,0), (0,1)],
              let (x', y') = (x+dx, y+dy),
              x' > 0 && x' <= w,
              y' > 0 && y' <= h]

-- when inserting a path between two cells, we need to 
insertPath :: MazeLoc -> MazeLoc -> Maze -> Maze
insertPath l1 l2 mz@(Maze _ cells) = undefined
\end{code}


And to help us visualize the mazes we generate, some drawing functions (which
return ASCII art strings):

\begin{code}
drawWallsNW :: MazeLoc -> Maze -> (String, String)
drawWallsNW c@(x, y) (Maze (w, h) cells) = 
  let nc = (x, y-1)
      wc = (x-1, y)
      adj = findWithDefault [] c cells
  in ("+" ++ if nc `elem` adj then "  " else "--",
      (if wc `elem` adj then " " else "|") ++ "  ")

drawMaze :: Maze -> String
drawMaze m@(Maze (w, h) _) = (concat $ map drawRow $ chunksOf w drawnCells) 
                             ++ bot
  where drawRow cs = let (l1, l2) = unzip cs
                     in concat l1 ++ "+\n" ++ concat l2 ++ "|\n"
        drawnCells = [drawWallsNW (x, y) m | y <- [1..h], x <- [1..w]]
        bot = (concat $ replicate w "+--") ++ "+"
\end{code}


-- Random values and State

Before we think about generating random mazes, we need to think about how to
generate random numbers. How would an API for obtaining random numbers look?

How's this API for a function that takes an input N and returns a random number
in the range [0,N)?

    randomRange :: Int -> Int

---

The only way for a function to return "random" values is by being stateful ---
i.e., by being passed some state to use as the basis for returning a random
number alongside an updated state. This is the basis for pseudo-random number
generators (PRNGs). In this context, we often to the state as a "seed" value.

A simple way of updating seed values is the Lehmer RNG algorithm:

    seed' = a*seed `mod` p

where `p` is a prime number (we won't worry about how to pick `a`).

---

Let's write a version of `randomRange` that works on this principle. We define
the `Seed` type to differentiate the seed value from the range. We rely on `mod`
to help us compute a pseudo-random value based on the input seed that is in the
specified range, too.

\begin{code}
type Seed = Int

randomRange :: Seed -> Int -> (Int, Seed)
randomRange max seed = undefined
\end{code}


To generate a series of random numbers, we need to chain together calls to
`randomRange`, passing along the new seeds from call to call:

\begin{code}
fourRands :: Int -> Seed -> [Int]
fourRands max s0 = let (v1, s1) = randomRange max s0
                       (v2, s2) = randomRange max s1
                       (v3, s3) = randomRange max s2
                       (v4, _)  = randomRange max s3
                   in [v1, v2, v3, v4]
\end{code}


This is a natural fit for the State monad. We can use a State monad to carry
along and update the seed in order to help us extract new random values when needed:

\begin{code}
getRandomRange :: Int -> State Seed Int
getRandomRange max = undefined


nRands :: Int -> Int -> State Seed [Int]
nRands = undefined
\end{code}


This is how the built-in random number library (System.Random) works. Here are
some relevant classes, instances, and functions:

    class Random a where
      randomR :: RandomGen g => (a, a) -> g -> (a, g)
      random :: RandomGen g => g -> (a, g)
      randomRs :: RandomGen g => (a, a) -> g -> [a]
      randoms :: RandomGen g => g -> [a]

    shuffle' :: RandomGen gen => [a] -> Int -> gen -> [a]

    instance Random Integer
    instance Random Int
    instance Random Double
    instance Random Char
    instance Random Bool

    class RandomGen g where
      next :: g -> (Int, g)
      genRange :: g -> (Int, Int)
      split :: g -> (g, g)

    instance RandomGen StdGen

    mkStdGen :: Int -> StdGen
    newStdGen :: IO StdGen


Let's write some functions that allow us to generate random values and shuffle
lists using a StdGen --- effectively our PRNG state --- pulled out of a State monad:

\begin{code}
getRandom :: (Int, Int) -> State StdGen Int
getRandom range = undefined

getShuffled :: [a] -> State StdGen [a]
getShuffled l = undefined
\end{code}


And now we're ready to implement a random maze generator!

We're going to use a maze generation algorithm known as *reursive backtracking*,
which starts with a fully "closed" maze (i.e., no cells reachable from any
other), and proceeds as follows:

  1. Pick a starting cell to visit -- this is the "current" cell.

  2. Pick a neighboring cell that has yet to be visited and connect it to the
     current cell. The neighboring cell becomes the new current cell.

  3. Keep repeating (2) until the current cell's neighbors have all been
     visited, then back up to the previous current cell and continue. 

  4. We're done when we back up to the starting cell.


\begin{code}
-- maze generator using recursive backtracking, using the state monad
genMaze :: MazeDims -> State StdGen Maze
genMaze dims = undefined


-- convenience functions for creating and drawing random mazes

randomMaze :: MazeDims -> IO Maze
randomMaze dims = evalState (genMaze dims) <$> newStdGen

drawRandomMaze :: MazeDims -> IO ()
drawRandomMaze dims = do gen <- newStdGen
                         let mz = evalState (genMaze dims) gen
                         putStrLn $ drawMaze mz
\end{code}
