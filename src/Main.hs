-- https://github.com/haskell-gi/haskell-gi/blob/master/examples/advanced/GstHelloWorld.hs
-- in the Declarative Gtk+ style of https://wickstrom.tech/programming/2018/09/04/declarative-gtk-programming-with-haskell.html

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
module Main where

-- This is a translation of the Hello World gstreamer example to haskell
-- Original C source:
-- https://gstreamer.freedesktop.org/documentation/application-development/basics/helloworld.html
-- Written by Jaro Reinders on 2017-01-09
--
-- All comments are copied from the source C code

import Control.Monad (void, when)
import System.Environment
import System.IO (stderr)
import System.Exit (exitFailure)
import Data.Text (pack, unpack, Text)
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe)

import Engine (gstreamer)
import Model (extract, seek, pos, peeks, matrix1)

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs

  -- putStrLn . unpack $ extract $ seek (1,0) $ pos (0,1) matrix1
  -- putStrLn . unpack $ peeks (\(a,b) -> (b,a)) $ pos (0,1) matrix1

  -- Check input arguments
  when (length args /= 1) $ do
    T.hPutStrLn stderr $ pack "Usage: " <> pack progName <> pack " <Video filename>"
    exitFailure

  -- putStrLn $ show $ ((extract routing) :: Outputs)
  -- putStrLn $ show $ (Nil :: Outputs)

  gstreamer args 
