{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}

-- Using HHFree seems to be able to give us:
--  - No need to create Profunctor and Category instances for a base functor
--  - Performance because of CPS
--  - A more general abstraction (specialised to Arrows)
--  - seems like a cleaner way to add effects to Free Arrows (than the extensible arrow approach)
--  - maybe a path towards a Dunai way to stack effects?

-- Compared to the FreeA from Extensible Free Arrows (https://www.youtube.com/watch?v=msQiLyExM3w)
-- where
--   eff a b -> arr a b is "interpretation"
-- while
--   FreeA eff -> Arrow is "compilation" ?

-- evalKleisliA forall m a b. (Monad m) => FreeA (Kleisli m) a b -> Kleisli m a b

-- Kleisli IO () () == () -> IO () === IO ()

-- Algebra / CoAlgebra? HHFreeA -> Arrow / Arrow -> HHFreeA ?

-- compileA compileA 
--   :: forall eff arr a b
--    . (Arrow arr) 
--   => (eff :~~> arr) 
--   -> HHFree Arrow eff a b 
--   -> arr a b
-- compileA ==? runHHFree

-- runKleisli (evalKleisliA $ compileA (interpPrintx <$> interpStoreXToFile) extensibleArrow) ("init")

module HHFreeArrow where

import Prelude hiding (id, (.))
import Data.Functor.HHFree 
import Control.Arrow
import Control.Category
import Data.Functor
import Data.Functor.Identity
import Data.Profunctor
import FreeArrows (Labelled(Labelled, unLabelled))
import Data.Monoid
import Data.Coerce (coerce)

type FreeA eff a b = HHFree Arrow eff a b
-- eff gets interpreted or compiled into Arrows.
-- eff could be a base functor (product or sum of other base functors)]
--   load :: Component () Int

-- eff could be a mtl style type (row of effects)
--   load :: (eff :>+: PrintX) => FreeA eff Text ()

-- Both could be used in a program like so:
--   program = proc \a $ do 
--     a <- load -< ()
--     returnA () 
 
--  SuperClass1 (* -> * -> *) (Category *) c => Category * (HHFree c f)
-- In FreeA f (i.e. HHFree Arrow f) we borrow the category instance of arrow
-- whatever the f (which is what we wanted to avoid having to write the 
-- instances for f - which we had to do with the simpler FreeArrow type)

-- Hypothesis is that :- nil in FreeArrow 
--                 == .  id  in Category (HHFree Arrow f)

-- 

compileA :: Arrow arr => (eff :~~> arr) -> FreeA eff a b -> arr a b
compileA f a = runHHFree a f

-- evalA is really like CPS apply (e.g. cpsStr id) except that we have to
-- unwrap our FreeA eff a b to get to the function which takes the continuation.

effect :: eff a b -> FreeA eff a b
effect a = HHFree $ \k -> k a

-- -- | The higher order free functor over two type parameters for constraint @c@.
-- newtype HHFree c f a b = HHFree { runHHFree :: forall g. c g => (f :~~> g) -> g a b }

-- -- | Bifunctorial Natural transformations.
-- type f :~~> g = forall a b. f a b -> g a b

-- nil :: Profunctor p => FreeA p a a
-- nil = PureP id

nil :: HHFree Arrow eff a a
nil = HHFree $ const id


data HIdentity :: * -> * -> * where
  HIdentity :: a -> b -> HIdentity a b



-- We don't want to have compilation be fixed at arrow construction time
-- So like Free monads, we want to pass something like const or id, or... 
compileIdentityA :: (HIdentity () () -> t) -> t
compileIdentityA hid2Arrow = hid2Arrow (HIdentity () ())

-- compileA 
--   :: forall eff arr a b
--    . (Arrow arr) 
--   => (eff :~~> arr) 
--   -> HHFree Arrow eff a b 
--   -> arr a b
-- compileA = go
--   where
--     go :: forall a b. (Arrow arr) => HHFree Arrow eff a b -> arr a b
--     go hhFreeA = _a 

-- data MSF m a b = MSF (a -> m (b, ?))
-- data ?SF w m a b = ?SF (w a -> m b)
-- data ?SF w (ReaderT Int m) a b = ?SF (w a -> m (b, Int))



-- HHFree Arrow eff a b === FreeA eff a b
-- HHFree (forall g. Arrow arr => (eff :~~> arr) -> arr a b)
-- HHFree (forall g. c g       => (f   :~~> g  ) -> g   a b)
thing :: FreeA eff () ()
--               (f :~~> g) -> g a b == (f :~~> g) (f () ())
thing = HHFree $ const id -- compileIdentityA

-- In this instance, we use Component as a "pattern functor"
-- rather than use mtl style effects, which would require an 
-- extra interpretation step.
data Component a b = Load ((a, Int) -> b)   -- GetLine (String -> x)
                   | Store (a -> (b, Int))
                   | Inc (a -> b)           -- Choice of opaque interpreter


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

-- program :: HHFree Arrow Component () ()
program :: HHFree Arrow HIdentity () ()
program = proc () -> do
  a <- thing -< ()
  returnA -< ()

-- foldFreeA :: (Profunctor p, Arrow a) =>
--               (forall b c.p b c -> a b c) -> FreeA p b c -> a b c

load :: HHFree g Component () Int
load = HHFree $ \k -> k $ Load (\(_, a) -> a) -- toCPS $ Load snd

-- -- load :: Category eff => FreeA eff () Int
-- -- load = HHFree $ \k -> k $ id $ _a -- \(_, a) -> a 

store :: HHFree g Component Int ()
store = HHFree $ \k -> k $ Store (\a -> ((), a))

inc :: HHFree g Component a a
inc = HHFree $ \k -> k $ Inc id


test :: HHFree Arrow Component () ()
test = proc () -> do
    () <- inc   -< ()
    a  <- load  -< ()
    b  <- load  -< ()
    () <- store -< (a + b)
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
                                  
exec :: Component :~~> StateA Int
exec (Load g) = StateA $ \(a, s) -> (g (a, s), s)
exec (Store g) = StateA $ \(a, _) -> g a
exec (Inc g) = StateA $ \(a, s) -> (g a, s+1)

-- data Graph e a = Empty
--                | Vertex a
--                | Connect e (Graph e a) (Graph e a)

-- data List a = Nil | Cons a (List a)



-- data HList a b = Nil | Cons a (HList a b)
--   deriving Show

-- -- instance Category HList where
-- --   id = Nil
-- --   -- (.) :: forall b c a. HList b c -> HList a b -> HList a c
-- --   _ . l = coerce l

-- -- instance Arrow HList where


-- execL :: Component :~~> HList 
-- execL (Load g)  = Nil
-- execL (Store g) = Nil
-- execL (Inc g)   = Nil


-- Do we need function arrows here or could we have Inc p a b?
-- Could we have Inc (a -> m b) ?

-- instance Profunctor Component where
--     lmap f (Load g) = Load $ \(a, s) -> g (f a, s)
--     lmap f (Store g) = Store (g . f)
--     lmap f (Inc g) = Inc (g . f)

--     rmap f (Load g) = Load (f . g)
--     rmap f (Store g) = Store $ \a -> let (b, t) = g a
--                                      in  (f b, t)
--     rmap f (Inc g) = Inc (f . g)


-- exec'' :: Num i => Component a b -> Labelled (Sum i) a b
-- exec'' (Load _) = Labelled (Sum 1)
-- exec'' (Store _) = Labelled (Sum 1)
-- exec'' (Inc _) = Labelled (Sum 2)

-- foldHHFreeA :: (Profunctor p, Arrow arr) => (p :~~> arr) -> HHFree Arrow HIdentity () () -> arr () ()
-- foldHHFreeA = undefined

-- data Graph a 
--   = Empty 
--   | Vertex a 
--   | Overlay (Graph a) (Graph a)
--   | Connect (Graph a) (Graph a)
--   deriving Show
 
-- arrowToGraph :: Arrow arr => arr () () -> Graph ()
-- arrowToGraph _ = Vertex ()

-- cat b c -> cat a b -> cat a c
-- 

-- 


-- data CGraph '[a] = Edge a0 a1 (CGraph '[a])
--                  | Edge a1 a2
--                  | ...
--                  | Edge ai-1 ai

-- . :: CGraph '[b, c, r...] -> CGraph '[a, b, r'...] -> CGraph '[a, c, (r + r' - b)...] 

-- . :: (r' :>+: b :>+: c) -> (r :>+: a :>+: b) -> (r - b /\ r' -b) :>+: a :>+: c
-- . :: (r :>+: b :>+: c) -> (r :>+: a :>+: b) -> r :>+: a :>+: c

--            int' includes b and c
-- . :: Graph int' b c -> Graph int a b -> Graph (int /\ int') a c

-- with the same int
-- . :: Graph b c -> Graph a b -> Graph a c

-- data Graph a b = Edge a b
--                | forall x. Graph a x :- Graph x b
--                | forall x y r. Graph (x, r) (y, r) :+ Graph (x, r) (y, r)

data Graph a b where
  Edge :: a -> b -> Graph a b
  (:-) :: Graph a b -> Graph b c -> Graph a c
  (:+) :: Graph a b -> Graph c d -> Graph (a,c) (b,d)

instance (Show a, Show b, forall x. (Show x) => Show ()) => Show (Graph a b) where
  show (Edge x y) = show x <> " <-> " <> show y
  show (Edge x y :- g') = show x <> " <-> " <> show y <> " <--> " <> show g'
  show (g :- Edge x y) = show g <> " <--> " <> show x <> " <-> " <> show y
  show (_ :- _) = "general"
  show (g :+ g') = show g <> "\n" <> show g'

g1 :: Graph () ()
g1 = Edge () ()

g2 :: Graph () ()
g2 = Edge () () :- Edge () ()

g3 :: Graph () ()
g3 = Edge () 0 :- Edge 0 ()

g4 :: Graph () ()
g4 = Edge () (0,1,2) :- Edge (0,1,2) (3,4,5) :- Edge (3,4,5) ()

g5 :: Graph () ()
g5 = Edge () (0,1) :- g5' :- Edge (3,4) ()

g5' :: Graph (Int, Int) (Int, Int)
g5' = Edge 0 3 :+ Edge 1 4  -- Edge (0,1,2) (3,4,5)

-- data Graph a b where
--   Edge :: a -> b -> Graph a b
--   Connect :: a -> b -> Graph b c -> Graph a c
--   Overlay :: a -> b -> Graph c d -> Graph (a,c) (b,d)

-- instance (Show a, Show b) => Show (Graph a b) where
--   show (Edge x y) = show x <> " <-> " <> show y
--   show (Connect x y g) = show x <> " <-> " <> show y <> " <--> " <> show g
--   show (Overlay x y g) = show x <> " <-> " <> show y <> "\n" <> show g

-- g1 :: Graph () ()
-- g1 = Edge () ()

-- g2 :: Graph () ()
-- g2 = Connect () () $ Edge () ()

-- g3 :: Graph () ()
-- g3 = Connect () 0 $ Edge 0 ()

-- g4 :: Graph () ()
-- g4 = Connect () (0,1,2) $ Connect (0,1,2) (3,4,5) $ Edge (3,4,5) ()

-- g5 :: Graph () ()
-- g5 = Connect () (0,1) $ g5' -- Edge (3,4) ()

-- g5' :: Graph (Int, Int) (Int, Int)
-- g5' = Edge 0 3 :+ Edge 1 4  -- Edge (0,1,2) (3,4,5)

-- instance Category (Graph i) where
--   G
--   g1 . g2 = ...

-- myGraph :: CGraph '[Int, Bool, Char, Int, Bool, Char] 
--      -- :: Edge Int Char (Edge Int Bool)
-- myGraph = 

-- Open edge requiring an a / Open edge requiring a b.
-- data HGraph a b = HGraph (a -> b -> 

-- instance Category HGraph where

-- instance Arrow HGraph where

-- execG :: Component :~~> Graph 


hhmain :: IO ()
-- hhmain = print $ getSum $ unLabelled $ evalA exec'' program
hhmain = do
  print $ runStateA (compileA exec test) ((), 10)
  -- print $ arrowToGraph $ compileA execG test
