{-# LANGUAGE RankNTypes #-}
module HHCPS where

-- https://free.cofree.io/2020/01/02/cps/

-- So, a function of type 
-- f :: a -> b
-- 
-- would become 
-- f':: a -> (b -> r) -> r 
-- f' a k = k (f a)

-- f' a id = id (f a) = f a

-- in CPS, where 
--   b -> r
-- is the continuation.

toCPS :: a -> (forall r. (a -> r) -> r)
toCPS = flip ($)

fromCPS :: (forall r. (a -> r) -> r) -> a
fromCPS = ($ id)

--            a  -> (b ->  r ) ->  r
listToCPS :: [a] -> (() -> [a]) -> [a]
listToCPS as = (as <>) . ($ ())

listFromCPS :: ((() -> [a]) -> [a]) -> [a]
listFromCPS cps = cps (const mempty)

data Tree = Branch Tree Tree | Leaf Int


leafSum :: Tree -> Int
leafSum (Leaf x) = x
leafSum (Branch l r) = leafSum l + leafSum r

leafSumCPS :: Tree -> (Int -> r) -> r
leafSumCPS (Leaf x) k = k x
leafSumCPS (Branch l r) k =
  leafSumCPS l $ \vl ->
    leafSumCPS r $ \vr ->
      k (vl + vr)

-- leafSumCPS :: Tree -> (Int -> r) -> r
-- leafSumCPS (Leaf x) return = return x
-- leafSumCPS (Branch l r) return =
--   leafSumCPS l $ \vl ->
--     leafSumCPS r $ \vr ->
--       return (vl + vr)

-- go (Branch (Leaf 3) (Branch (Leaf 2) r)) return = ...

-- CPS (in addition to stack safe computations) can be used for mixing 
-- pattern matching and control flow.

-- leafSumCPS (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) id
-- leafSumCPS (Leaf 1) $ \vl ->
--   leafSumCPS (Branch (Leaf 2) (Leaf 3)) $ \vr ->
--      id (vl + vr)
-- \vl -> ... $ 1

-- leafSumCPS (Leaf 2) $ \vl ->
--   leafSumCPS (Leaf 3) $ \vr' ->
--      (\vr ->
--         id (1 + vr)) (vl + vr')

-- \vl -> ... $ 2
-- leafSumCPS (Leaf 3) $ \vr' ->
--      (\vr ->
--         id (1 + vr)) (2 + vr')

-- \vr' -> ... $ 3
-- (\vr -> id (1 + vr)) (2 + 3)
-- (\vr -> id (1 + vr)) 5
-- id (1 + 5)
-- 6

-- forall g. c g => (f :~~> g) -> g a b
-- forall r. c r => (a :~~> r) -> r x y
-- forall r. c r => (a x' y' -> r x' y') -> r x y
--                  (a       -> r      ) -> r
--                             (a       -> ((b -> r) -> r)
-- forall r.                   (a x' y' ->  (b x'' y'' -> r x'' y'') -> r x' y')
-- forall r. c r =>            (a x' y' ->  (b :~~> r) -> r x' y')
-- forall h. c h =>            (f a b -> (g :~~> h) -> h c d)

-- bind for profunctor monad https://hackage.haskell.org/package/profunctors-5.2/docs/Data-Profunctor-Monad.html#t:ProfunctorMonad
-- chainCPSA 
--   :: forall f g r
--    . (c g, c h) 
--   =>           (f :~~> r) -> r a b   --       m f
--   -> (f a b -> (g :~~> r) -> r a b)  --       f -> m g
--   ->           (g :~~> r) -> r a b   --       m g

chainCPS 
  ::        ((a -> r) -> r)   -- Cont a       -- m a
  ->  (a -> ((b -> r) -> r))  -- a -> Cont b  -- a -> m b
  ->        ((b -> r) -> r)   -- Cont b       -- mb
chainCPS s f = s . flip f

hhcpsmain :: IO ()
-- hhmain = print $ getSum $ unLabelled $ evalA exec'' program
hhcpsmain = do
  print "toCPS"
  let str = "abc"
      cpsStr = toCPS str
  print $ cpsStr id
  print "leafSum"
  let tree = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
  print $ leafSum tree
  print $ leafSumCPS tree id
  print "listToCPS"
  let list = [1,2,3,4,5]
      cps = listToCPS list
      -- ([1,2,3,4,5] <>) . ($ ()) f -- f :: (() -> [a])  
  print $ cps (const [10])
  let list1 = [1]
      list2 = [2]
      list3 = [3]
      cps' = listToCPS []
                `chainCPS` \() -> listToCPS list1  
                `chainCPS` \() -> listToCPS list2
                `chainCPS` \() -> listToCPS list3
  print $ cps' (const [10])
  print "yo"
  return ()