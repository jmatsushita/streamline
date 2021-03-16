
{-# LANGUAGE OverloadedLabels  #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
module Model where

-- This is a translation of the Hello World gstreamer example to haskell
-- Original C source:
-- https://gstreamer.freedesktop.org/documentation/application-development/basics/helloworld.html
-- Written by Jaro Reinders on 2017-01-09
--
-- All comments are copied from the source C code

import Control.Monad (void, when)

import Data.Maybe (fromMaybe)
import Haskus.Utils.Variant
import Haskus.Utils.EADT
import Haskus.Utils.ContFlow
import Data.Text (pack, unpack, Text)

class Functor w => Comonad w where 
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  -- extend :: (a -> w b) -> w a -> w b

data Store k v = Store k (k -> v)

instance Functor (Store k) where
  fmap g (Store k f) = Store k (g . f)

instance Comonad (Store k) where
  extract (Store k f) = f k
  duplicate (Store k f) = Store k (flip Store $ f) 

type Matrix a = Store (Int, Int) a

pos :: k -> Store k a -> Store k a
pos k (Store _ f) = Store k f

-- seek ignores the current key, it's an absolute query
peek :: k -> Store k a -> a
peek k s = extract (pos k s)

-- relative query
peeks :: (k -> k) -> Store k a -> a
peeks g (Store k f) = extract (Store (g k) f)

-- absolute mutation
seek :: k -> Store k a -> Store k a
seek k (Store _ f) = Store k f

-- relative mutation
seeks :: (k -> k) -> Store k a -> Store k a
seeks g (Store k f) = Store (g k) f

-- experiment 
--   :: Functor f 
--  => (k -> f k) -- 
--  -> Store k v
--  -> f a

-- Look again at the code for implementing Store as a Cofree comonad
-- Layering of Comonads for fractal introns/extrons... Cofree (Free (Store k ?)) ?
-- k as a composition of variants...
-- Store layering... Store comonad composition (Sum ks and Product ks)
-- data Layered [k] a = (Algebra k) => Store k a



matrix1 :: Matrix Text
matrix1 = Store (0,0) f
  where
    f :: (Int, Int) -> Text
    f (0, 0) = pack "Zero"
    f (0, 1) = pack "One"
    f (1, 0) = pack "Also One"
    f _ = pack "Anything Else"


matrix2 :: Matrix Text
matrix2 = Store (0,0) f
  where
    f :: (Int, Int) -> Text
    f (0, 0) = pack "Zero"
    f (0, 1) = pack "One"
    f (1, 0) = pack "Also One"
    f _ = pack "Anything Else"

-- matrix1 :: Matrix Text
-- matrix1 = Store f (0,0)
--   where
--     f :: (Int, Int) -> Text
--     f (0, 0) = pack "Zero"
--     f (0, 1) = pack "One"
--     f (1, 0) = pack "Also One"
--     f _ = pack "Anything Else"

x,y :: V '[String,Int]
x = V "test"
y = V @Int 10



data VideoF name source e = VideoF name source e deriving (Functor)
data AudioF name source e = AudioF name source e deriving (Functor)
data DataF  name source e = DataF  name source e deriving (Functor)
data NilF               e = NilF            deriving (Functor)

class PrettyPrint f where
  prettyPrint' :: f String -> String

instance PrettyPrint (VideoF String s) where
  prettyPrint' (VideoF s _ e) = mconcat [s,e] 


instance PrettyPrint (AudioF String s) where
  prettyPrint' (AudioF s _ e) = mconcat [s,e] 


instance PrettyPrint (DataF String s) where
  prettyPrint' (DataF s _ e) = mconcat [s,e] 

-- prettyPrint :: BottomUp PrettyPrint xs String => EADT xs -> String
-- prettyPrint e = bottomUp (toBottomUp @PrettyPrint prettyPrint') e

pattern Video :: VideoF name source :<: xs => name -> source -> EADT xs -> EADT xs
pattern Video n s l = VF (VideoF n s l)
pattern Audio :: AudioF name source :<: xs => name -> source -> EADT xs -> EADT xs
pattern Audio n s l = VF (AudioF n s l)
pattern Data :: DataF name source :<: xs => name -> source -> EADT xs -> EADT xs
pattern Data n s l = VF (DataF n s l)

pattern Nil :: NilF :<: xs => EADT xs
pattern Nil = VF NilF

-- type URL = String
data AudioInput = AudioInput String
data VideoInput = VideoInput String 
data DataInput  = DataInput String 

data AudioOutput = AudioOutput String
data VideoOutput = VideoOutput String 
data DataOutput  = DataOutput String 

type Name = String

type Inputs = EADT '[ AudioF Name AudioInput
                    , VideoF Name VideoInput
                    , DataF Name DataInput
                    , NilF]

type Outputs = EADT '[ AudioF Name AudioOutput
                   , VideoF Name VideoOutput
                   , DataF Name DataOutput
                   , NilF]
bassSceneInputs :: Inputs

bassSceneInputs 
  = Audio "live bass" (AudioInput "ableton live output 1") 
  $ Audio "backing track" (AudioInput "ableton live output 2") 
  $ Video "close up cam" (VideoInput "camlink DSLR") 
  $ Data "tabs" (DataInput "gp4 file") 
  $ Nil

bassSceneOutputs :: Outputs
bassSceneOutputs 
  = Audio "stream" (AudioOutput "twitch")
  $ Video "stream" (VideoOutput "twitch") 
  $ Video "recording" (VideoOutput "twitch") 
  $ Data "tab" (DataOutput "hyperswarm") 
  $ Nil

type Routing = Store Inputs Outputs

routing :: Routing 
routing = Store bassSceneInputs route
  where 
    route :: Inputs -> Outputs
    route activeInputs = cata cont activeInputs

    cont l = l >:>
      ( \(AudioF n s r) -> Audio "stream" (AudioOutput "twitch") $ Nil
      , \(VideoF n s r) -> r
      , \(DataF n s r) -> r
      , \NilF        -> Nil
      )

