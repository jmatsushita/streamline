{-# OPTIONS -W #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

-- http://blog.sigfpe.com/2017/01/building-free-arrows-from-components.html

module FreeArrows where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Data.Profunctor
import Data.Monoid

infixr :-

-- There's also a representation based on snoc lists
data FreeA p a b = PureP (a -> b)                        -- Nil
                 | forall x. p a x :- FreeA p x b        -- a : List a

instance Show (FreeA p a b) where
  show (PureP _) = "PureP"
  show pab = show pab

-- data Compose f g a b = forall x. Compose (f a x) (g x b)
-- Compose (->) p a b <-> forall x. Compose (a -> x) (p x b) <-> p a b
--

nil :: Profunctor p => FreeA p a a
nil = PureP id

instance Profunctor p => Profunctor (FreeA p) where
  -- lmap :: (a -> b) -> p b c -> p a c
  lmap f (PureP g) = PureP (g . f)
  lmap f (g :- h)  = lmap f g :- h
  -- rmap :: (b -> c) -> p a b -> p a c
  rmap f (PureP g) = PureP (f . g)
  rmap f (g :- h) = g :- rmap f h

instance Strong p => Strong (FreeA p) where
    first' (PureP f) = PureP (first' f)
    first' (f :- g) = first' f :- first' g

instance Profunctor p => Category (FreeA p) where
    id = PureP id
    g . PureP f = lmap f g
    k . (g :- h) = g :- (k . h)

instance (Profunctor p, Strong p) => Arrow (FreeA p) where
     arr = PureP
     first = first'

foldFreeA :: (Profunctor p, Arrow a) =>
              (forall b c.p b c -> a b c) -> FreeA p b c -> a b c
foldFreeA _ (PureP g) = arr g
foldFreeA f (g :- h) = foldFreeA f h . f g

data Component a b = Load ((a, Int) -> b)   -- GetLine (String -> x)
                   | Store (a -> (b, Int))
                   | Inc (a -> b)           -- Choice of opaque interpreter

-- Do we need function arrows here or could we have Inc p a b?
-- Could we have Inc (a -> m b) ?

instance Profunctor Component where
    lmap f (Load g) = Load $ \(a, s) -> g (f a, s)
    lmap f (Store g) = Store (g . f)
    lmap f (Inc g) = Inc (g . f)

    rmap f (Load g) = Load (f . g)
    rmap f (Store g) = Store $ \a -> let (b, t) = g a
                                     in  (f b, t)
    rmap f (Inc g) = Inc (f . g)

instance Strong Component where
    first' (Load g) = Load $ \((a, x), s) -> (g (a, s), x)
    first' (Store g) = Store $ \(a, x) -> let (b, t) = g a
                                          in  ((b, x), t)
    first' (Inc g) = Inc (first' g)

add :: Num a => FreeA Component (a, a) a
add = PureP $ uncurry (+)

load :: FreeA Component () Int
load = Load (\(_, a) -> a) :- nil

store :: FreeA Component Int ()
store = Store (\a -> ((), a)) :- nil

inc :: FreeA Component a a
inc = Inc id :- nil

test :: FreeA Component () ()
test = proc () -> do
    () <- inc   -< ()
    a  <- load  -< ()
    b  <- load  -< ()
    c  <- add   -< (a, b)
    () <- store -< c
    returnA -< ()

-- the State Arrow.
newtype StateA s a b = StateA { runStateA :: (a, s) -> (b, s) }

instance Category (StateA s) where
    id = StateA id
    StateA f . StateA g = StateA (f . g)

instance Arrow (StateA s) where
    arr f = StateA $ \(a, s) -> (f a, s)
    first (StateA g) = StateA $ \((a, x), s) -> let (b, t) = g (a, s)
                                      in  ((b, x), t)
                                  
exec :: Component a b -> StateA Int a b
exec (Load g) = StateA $ \(a, s) -> (g (a, s), s)
exec (Store g) = StateA $ \(a, _) -> g a
exec (Inc g) = StateA $ \(a, s) -> (g a, s+1)

-- What about Pairing in this setting? 
-- There is most probably a CofreeA that we can build interpreters from!!!
-- Would that be a pairing between profunctors?
-- CoArrow is an Arrow no? Whereas a CoMonad is not a Monad.

-- In the Free Monad / CoFree Comonad world you compose with Products and Sums, Day convolution
-- In the Free Arrow / CoFree Arrow what do you compose with?

-- https://math.stackexchange.com/questions/2609863/cofree-construction-thought-process/2610009

-- run :: PairA Component CoComponent 
--     => Component a b -> CoComponent a b -> StateA Int a b

-- data CoComponent a b = CoComponent { load  :: (a -> (b, Int))   
--                                    , store :: ((a, Int) -> b)
--                                    , inc   :: (Int -> Int) 
--                                    }

-- sigfpe's comment:
-- > Here I found a certain amount of flexibility in how much you store in the Component and how much is deferred to the interpreter.

-- the pairing might actually give a principled way to choose a representation...!

exec' :: Component a b -> Kleisli IO a b
exec' (Load g) = Kleisli $ \a -> do
    putStrLn "What is your number now?"
    s <- fmap read getLine
    return $ g (a, s)
exec' (Store g) = Kleisli $ \a -> do
    let (b, t) = g a
    putStrLn $ "Your number is now " ++ show t ++ "."
    return b
exec' (Inc g) = Kleisli $ \a -> do
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
exec'' (Load _) = Labelled (Sum 1)
exec'' (Store _) = Labelled (Sum 1)
exec'' (Inc _) = Labelled (Sum 2)


-- foldFreeA :: (Profunctor p, Arrow a) =>
--               (forall b c. p b c -> a b c) -> FreeA p b c -> a b c

main = do
    print $ runStateA (foldFreeA exec test) ((), 10)
    putStrLn "Your number is 10." >> runKleisli (foldFreeA exec' test) ()
    print $ getSum $ unLabelled $ foldFreeA exec'' test