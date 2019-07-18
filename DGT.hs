{- | SCCs in random digraphs, as a test case for containers:Data.Graph

for mathematical background, and expected results, see 

Karp (1990, "The transitive closure of a random digraph")
Luczak (1990, "The phase transition in the evolution of random digraphs")

cited in https://math.stackexchange.com/questions/353800/what-is-the-expected-size-of-the-largest-strongly-connected-component-of-a-graph

-}

{-# language GeneralizedNewtypeDeriving #-}

import qualified Data.Graph as G
import qualified Data.Graph.SCC as S
import qualified Data.Array as A

import Gauge (bgroup, bench, defaultMain, whnf)

main = let base = 10 in defaultMain
  [ bgroup "buildG" $ do
      e <- [ 1 .. 6 ] ; let n = base^e :: Int
      return $ bgroup ("n="<> show base <> "^" <> show e) $ do
        p <- [ 0.5 ] --  [0, 0.1 .. 1 ]
        return $ bench ("p=" <> show p)
          $ whnf (sum . concat . A.elems . random_directed_graph n p) 42
  , bgroup "scc" $ do
      e <- [ 1 .. 6 ] ; let n = base^e :: Int
      return $ bgroup ("n="<> show base <> "^" <> show e) $ do
        p <- [ 0.5 ] --  [0, 0.1 .. 1 ]
        return $ bgroup ("p=" <> show p) $ do
          (name,fun) <- [ ("containers",scc_sizes)
                        , ("GraphSCC",scc_sizes')
                        ]
          return $ bench name
            $ whnf (sum . fun) (random_directed_graph n p 42)
  ]

scc_sizes :: G.Graph -> [Int]
scc_sizes g = counting_sort (A.bounds g) $ map length $ G.scc g

scc_sizes' :: G.Graph -> [Int]
scc_sizes' g = counting_sort (A.bounds g) $ map length $ S.sccList g

counting_sort :: (Int,Int) -> [Int] -> [Int]
counting_sort (lo,hi) xs = do
  (k,v) <- A.assocs $ A.accumArray (+) 0 (lo,hi) $ zip xs $ repeat 1
  replicate v k

random_directed_graph
  :: Int -- ^ number of vertices
  -> Double -- ^ edge probability
  -> Seed
  -> G.Graph
random_directed_graph n p s = G.buildG (1,n)
  $ map snd
  $ filter (\(r,e) -> r < p)
  $ zip (random_doubles s)
  $ (,) <$> [1..n] <*> [1..n]
  

newtype Seed = Seed Int deriving Num

-- | lazy list of pseudo-random numbers from linear congruential generator,
-- coefficients taken from "the BSD rand generator", as cited in
-- https://www.gnu.org/software/gsl/doc/html/rng.html#c.gsl_rng_rand
random_ints :: Seed -> [Int]
random_ints (Seed s) =
  iterate (\x -> mod (1103515245 * x + 12345) (2^31)) s

random_doubles :: Seed -> [Double]
random_doubles s =
  map (\i -> fromIntegral i / fromIntegral (2^31)) $ random_ints s
