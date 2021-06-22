{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Design where

import Data.Array (Array)
import Data.Text (Text)
import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Data.Profunctor
import Data.Monoid

import FreeArrows (FreeA(..), nil, foldFreeA)

data PadType = Audio | Video | Any
  deriving (Eq)
data PadNode = Init | PadNode Int PadType
  deriving (Eq)

-- newtype Pad = Pad Text

type Pad = Int

type Pads = Array Int Pad

-- data ElementEdge = Process Int | Source | Sink

newtype Element = Element Text

type Elements = Array Int Element

-- Alga is cool but:
--   - Do we have the decorated cospan structure here?
--   - Is it like in Arrow syntax where port get ids, then id reuse means wiring?
--       -> Is this node biased? (an edge is given to ports)
--   - alga would be edge biased ? (node ids are given to edges)
--      - given how alga fully connects subgraphs, it might not be practical for our use case.


--  Alga
--     E (V id) (G id) -- could be an vertex, could be connecting a whole subgraph.

--  Graphs -- labelled edges as tuple annotations
--     V, [(E, V)]  

--  Arrow - directed...
--      V <- E  <- V'
--      V <  E' <- 0

-- Free Arrows
-- https://stackoverflow.com/questions/12001350/useful-operations-on-free-arrows

-- data FreeA eff a b where
--     Pure :: (a -> b) -> FreeA eff a b
--     Effect :: eff a b -> FreeA eff a b -- MSF m a b = a -> m (b, MSF m a b)
--     Seq :: FreeA eff a b -> FreeA eff b c -> FreeA eff a c
--     Par :: FreeA eff a b -> FreeA eff c d -> FreeA eff (a, c) (b, d)

-- effect :: eff a b -> FreeA eff a b
-- effect = Effect

-- instance Category (FreeA eff) where
--     id = Pure id
--     (.) = flip Seq

-- instance Arrow (FreeA eff) where
--     arr = Pure
--     first f = Par f id
--     second f = Par id f
--     (***) = Par

-- This below is faster but harder for me to understand at this point. This is using CPS Style:
-- type x :~> y = forall a b. x a b -> y a b
-- newtype FreeA eff a b 
--  = FreeA
--    { runFreeA 
--      :: forall arr
--       . Arrow arr
--      => (eff :~> arr) 
--      -> arr a b 
--    }

-- evalA f a = runFreeA a f
-- effect a = FreeA $ \k -> k a

-- instance Category (FreeA f) where
--   id = FreeA $ const id
--   FreeA f . FreeA g = FreeA $ \k -> f k . g k

-- instance Arrow (FreeA f) where
--   arr f = FreeA $ const (arr f)
--   first (FreeA f) = FreeA $ \k -> first (f k)
--   second (FreeA f) = FreeA $ \k -> second (f k)
--   FreeA f *** FreeA g = FreeA $ \k -> f k *** g k
--   FreeA f &&& FreeA g = FreeA $ \k -> f k &&& g k

-- There's also https://hackage.haskell.org/package/free-category-0.0.4.3/docs/Control-Arrow-Free.html 


data Component    a           b 
  = Source      ((a, Pad) ->  b     )   -- GetLine (String -> x)
  | Sink         (a       -> (b, Pad))
  | Transformer ((a, Pad) -> (b, Pad))
  | Noop         (a       ->  b     )   -- Choice of opaque interpreter

-- MSF m a b = a -> m (b, MSF m a b)

instance Profunctor Component where
    lmap f (Source g) = Source $ \(a, s) -> g (f a, s)
    lmap f (Sink g) = Sink (g . f)
    lmap f (Transformer g) = Transformer $ \(a, s) -> g (f a, s)
    lmap f (Noop g) = Noop (g . f)

    rmap f (Source g) = Source (f . g)
    rmap f (Transformer g) = Transformer $ \a -> let (b, t) = g a
                                     in  (f b, t)
    rmap f (Sink g) = Sink $ \a -> let (b, t) = g a
                                     in  (f b, t)
    rmap f (Noop g) = Noop (f . g)

instance Strong Component where
    first' (Source g) = Source $ \((a, x), s) -> (g (a, s), x)
    first' (Sink g) = Sink $ \(a, x) -> let (b, t) = g a
                                          in  ((b, x), t)
    first' (Transformer g) = Transformer $ \((a, x), s ) -> let (b, t) = g (a, s)
                                          in  ((b, x), t)
    first' (Noop g) = Noop (first' g)

instance Show (Component () Int) where
    show (Source _)      = "Source"
    show (Sink _)        = "Sink" 
    show (Transformer _) = "Transformer"
    show (Noop _)        = "Noop"

filesrc :: String -> FreeA Component () Pad
filesrc _ = Source snd :- nil

decode :: FreeA Component Pad Pad
decode = Transformer id :- nil

conv :: FreeA Component Pad Pad
conv = Transformer id :- nil

sink :: FreeA Component Pad ()
sink = Sink ((),) :- nil

-- -- decorated cospan
-- edge :: FreeA Component () ()
-- edge = 

type Time = Int

location :: Time -> String
location _ = "/home/jun/Videos/1.mp4"

-- time :: FreeA Component () Time
-- time = 

pipeline :: FreeA Component () ()
pipeline = proc () -> do
    -- t  <- time -< ()
    a  <- filesrc $ location 0 -< ()
    b  <- decode  -< a
    c  <- conv    -< b
    () <- sink    -< c
    returnA -< ()

-- GStreamer content format negotiation between elements might mean
-- that conv and sink (for instance) determine the type of `c`

-- How do we model time:
--   - For Element parameters (like location)
--   - For Edge terms (format negotiation)
--   - For the graph structure (Graph Patching Language)


-- exec :: Component a b -> Kleisli IO a b
-- exec (Source g) = Kleisli $ \a -> do
--     -- putStrLn "What is your number now?"
--     -- s <- fmap read getLine
--     let s = 1
--     return $ g (a, s)
-- exec (Sink g) = Kleisli $ \a -> do
--     let (b, t) = g a
--     putStrLn $ "Your number is now " ++ show t ++ "."
--     return b
-- exec (Transformer g) = Kleisli $ \a -> do
--     let (b, t) = g (a, 1)
--     putStrLn $ "Your number is now " ++ show t ++ "."
--     return b
-- exec (Noop g) = Kleisli $ \a -> do
--     putStrLn "Increment your number."
--     return $ g a


exec :: Component a b -> Kleisli IO a b
exec (Source g) = Kleisli $ \a -> do
    putStrLn "What is your number now?"
    s <- fmap read getLine
    return $ g (a, s)
exec (Sink g) = Kleisli $ \a -> do
    let (b, t) = g a
    putStrLn $ "Your number is now " ++ show t ++ "."
    return b
exec (Transformer g) = Kleisli $ \a -> do
    let (b, t) = g (a, 1)
    putStrLn $ "Your number is now " ++ show t ++ "."
    return b
exec (Noop g) = Kleisli $ \a -> do
    putStrLn "Increment your number."
    return $ g a


newtype Labelled m a b = Labelled { unLabelled :: m }

instance Monoid m => Category (Labelled m) where
    id = Labelled mempty
    Labelled a . Labelled b = Labelled (a `mappend` b)

instance Monoid m => Arrow (Labelled m) where
    arr _ = Labelled mempty
    first (Labelled m) = Labelled m

exec'' :: Num i => Component a b -> Labelled (Sum i) a b
exec'' (Source _)      = Labelled (Sum 1)
exec'' (Sink _)        = Labelled (Sum 1)
exec'' (Transformer _) = Labelled (Sum 2)
exec'' (Noop _)        = Labelled (Sum 0)

design :: IO ()
design = do
    -- print $ runStateA (foldFreeA exec test) ((), 10)
    putStrLn "Your number is 10." >> runKleisli (foldFreeA exec pipeline) ()
    -- print pipeline
    print $ getSum $ unLabelled $ foldFreeA exec'' pipeline

--  PHOAS Structured Graphs
--
--       ?????
--

-- Can we make this an Indexed Graph, i.e. heterogeneous graph?

-- data StatePipeline = Graph ElementEdge Pads

-- The holy grail here would be to derive a delta type from the state type.
-- Then be able to enumerate the delta type space, to for instance create the 
-- options in a dropdown, or even derive the dropdown from the delta type,
-- or even the whole component for editing the state, from the state type.

-- data StateAudioInput = Live HardwareId SamplingFreq Bitrate
--                      | Recorded URI SamplingFreq Bitrate



-- viewAudioInputCustom :: State -> Widget Event
-- viewAudioInputCustom = \case
--   Live hid _ _ ->
--     widget
--       Field [ #label := "Hardware Id"
--               #value := hid
--              , on #change $ hid
--             ]

-- updateAudioInputCustom :: State -> Event -> State
-- updateAudioInputCustom 

-- Dunai like thought
-- data MSF m a b = a -> m (b, MSF m a b)
-- SF :: MSF (ReaderT Time Identity) a b
--    :: a -> ReaderT Time Identity (b, MSF m a b)
--    :: a -> Reader Time (b, MSF m a b)
--    :: a -> (t -> (b, MSF m a b))
-- mtl
-- SF :: (MonadReader Time m) => a -> m (b, MSF m a b)

-- data MSF m a b = MSF (a -> m (b, ?))
