{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module GI.Gst.Declarative where

-- In GI.Gtk.Declarative: 
-- https://wickstrom.tech/programming/2018/09/04/declarative-gtk-programming-with-haskell.html
-- Rendering becomes a pure function from state to a declarative widget
-- 
-- Here:
-- Running becomes a pure function from state to a declarative pipeline
-- Graph patching -> Matrix routing patching (ALGA)
--   - Document Graph? Where Vertices have GSTElement properties?
--   - Signals?

-- Event handling will be a mapping from:
--   - Signals to  
--      - Event Data Type, 
--      - Base Functor for a Free Monad, 
--      - Sensor/Actuators for Dunai/Rhine
--   - ??? Are properties animatable in GES?
--   - Read/Write properties? Timecode.

-- widget Button / CheckButton
-- element FileSrc / DecodeBin3 (intead of "filesrc" "decodebin3")
--   such that FileSrc will be a source (in the directed graph sense)
--   Maybe an IndexedMonad abstraction would be the right one?
--   Graph representation.
--   Arrows... Free Arrows?
--   Essence of Dataflow programming comonads.

-- Maybe we want to aim for a gst-launch-1.0 pipeline style syntax.
-- FileSrc [#location := Path "home" </> "jun", on #start ShowPlayingStatus] 
--   <!> DecodeBin3 [] <!> ...

-- FileSrc [#location := Path "home" </> "jun", 
--   onM #changeStatus (ShowStatus . getCurrentStatus)] 


-- Early Linux (more evolved graph piping syntax)
-- MyBin. <sub pipeline...> 
-- Composability of pipelines, composability of directed graphs. (ALGA)

-- pads are objects in G
-- elements are arrows in G


-- Alga
-- data Graph e a = Empty
--                | Vertex a
--                | Connect e (Graph e a) (Graph e a)

-- https://hackage.haskell.org/package/graphs-0.7.1

import Data.Array
import Data.Text hiding (index)
import Data.Graph.AdjacencyList
-- import Data.Graph.AdjacencyMatrix
import Data.Graph.Algorithm
import Data.Graph.Algorithm.DepthFirstSearch


data PadType = Audio | Video | Any
  deriving (Eq)
data PadNode = Init | PadNode Int PadType
  deriving (Eq)

newtype Pad = Pad Text

type Pads = Array Int Pad

data ElementEdge = Process Int | Source | Sink

newtype Element = Element Text

type Elements = Array Int Element

initial :: Int
initial = minBound

term :: Int
term = maxBound

instance Ord PadNode where
  compare Init _ = GT
  compare (PadNode i  _) (PadNode j _) = compare i j

instance Ix PadNode where
  range :: (PadNode,PadNode) -> [PadNode]
  range (PadNode i t, PadNode j u)
    | t == u = flip PadNode t <$> range (i,j)
    | otherwise = flip PadNode Any <$> range (i,j)
  range (Init, Init) = []
  range (Init, PadNode i t) = Init : (flip PadNode t <$> range (0, i))

  index :: (PadNode, PadNode) -> PadNode -> Int
  index (PadNode i _, PadNode j _) (PadNode k _) = index (i,j) k

  inRange :: (PadNode, PadNode) -> PadNode -> Bool
  inRange (PadNode i _, PadNode j _) (PadNode k _) = inRange (i,j) k

pipeline :: Array PadNode (ElementEdge, [PadNode])
pipeline = array (Init, PadNode 1 Any)
  [ (Init, (Source, [PadNode 0 Any]))
  , (PadNode 0 Any, (Process 0, [PadNode 1 Any]))
  , (PadNode 1 Any, (Process 1, []))
  ]

-- newtype AdjacencyMatrix arr i a = AdjacencyMatrix { runAdjacencyMatrix :: arr (i,i) Bool -> a }


-- g1 :: Array PadNode [PadNode]
-- g1 = array (Init, PadNode 0 Any) 
--   [(Init, [PadNode 0 Any]), (PadNode 0 Any, [])]

g1 :: Array Int [Int]
g1 = array (0, 4) 
  [ (0, [1,3])
  , (1, [2]), (2, [0])
  , (3, [4]), (4, [])
  ]

data Orderings = Orderings
  {  enterV :: [Int]
  ,  enterE :: [(Int, Int)]
  ,  gray   :: [(Int, Int)]
  ,  exitV  :: [Int]
  ,  black  :: [(Int, Int)]
  } deriving Show

instance Semigroup Orderings where
 (Orderings a1 a2 a3 a4 a5) <> (Orderings b1 b2 b3 b4 b5) =
  Orderings (a1 ++ b1) (a2 ++ b2) (a3 ++ b3) (a4 ++ b4) (a5 ++ b5)

instance Monoid Orderings where
 mempty = Orderings [] [] [] [] []

orderings :: GraphSearch (AdjacencyList Int) Orderings
orderings = GraphSearch
  (\v -> return $ mempty {enterV = [v]})
  (\e -> return $ mempty {enterE = [e]})
  (\e -> return $ mempty {gray   = [e]})
  (\v -> return $ mempty {exitV  = [v]})
  (\e -> return $ mempty {black  = [e]})

dfsTest :: Orderings
dfsTest = f (dfs orderings 1) g1
 where
   f :: AdjacencyList Int Orderings -> Array Int [Int] -> Orderings
   f = runAdjacencyList 

askTest = runAdjacencyList f g1
  where 
    f :: AdjacencyList Int Text
    f = AdjacencyList { runAdjacencyList = 
      \arr -> pack $ mconcat $ show <$> elems arr }




-- newtype AdjacencyList i a = AdjacencyList { runAdjacencyList :: Array i [i] -> a }

-- ask :: AdjacencyList i (Array i [i])
-- ask = AdjacencyList id

-- g :: AdjacencyMatrix Array Int PadNode
-- g = AdjacencyMatrix $ 
--   \a -> 

newtype A a b = A { runA :: a -> b }

a :: A Int Bool
a = A (const True)

run :: Int -> Bool
run = runA a

-- data StateGraphPipeline = 