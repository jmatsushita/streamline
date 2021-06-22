
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Engine where

-- This is a translation of the Hello World gstreamer example to haskell
-- Original C source:
-- https://gstreamer.freedesktop.org/documentation/application-development/basics/helloworld.html
-- Written by Jaro Reinders on 2017-01-09
--
-- All comments are copied from the source C code

import qualified GI.GLib as GLib
import qualified GI.Gst  as Gst


import Data.GI.Base.Properties (setObjectPropertyString, getObjectPropertyString)

import Control.Monad (void, when)
import System.Environment
import System.IO (stderr)
import System.Exit (exitFailure)
import Data.Text (pack, unpack, Text)
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe, fromJust)
import GI.Gst

busCall :: GLib.MainLoop -> Gst.Bus -> Gst.Message -> IO Bool
busCall loop _bus message = do
  messageTypes <- Gst.getMessageType message
  -- print messageTypes

  -- when (Gst.MessageTypeStateChanged `elem` messageTypes) $ do
  --   src <- Gst.getMessageSrc message
  --   name <- case src of
  --     Just s -> do
  --       n <- getObjectPropertyString s "name"
  --       return $ fromMaybe "error" n
  --     Nothing -> return "no src"
  --   print "---"
  --   print name
  --   (a,b,c) <- Gst.messageParseStateChanged message
  --   print "---"
  --   print a
  --   print b
  --   print c
  --   print "..."

  when (Gst.MessageTypeWarning `elem` messageTypes) $ do
    src <- Gst.getMessageSrc message
    name <- case src of
      Just s -> do
        n <- getObjectPropertyString s "name"
        return $ fromMaybe "error" n
      Nothing -> return "no src"
    print $ "### " <> name
    -- details <- Gst.messageParseWarningDetails message
    -- T.hPutStrLn stderr . ("Warning: " <>) =<< Gst.gerrorMessage gerror
    print "warning"

  when (Gst.MessageTypeEos `elem` messageTypes) $ do
    print messageTypes
    -- (gerror,_debug) <- Gst.messageParseWarning message
    -- T.hPutStrLn stderr . ("Warning: " <>) =<< Gst.gerrorMessage gerror
    putStrLn "End of stream"
    GLib.mainLoopQuit loop

  when (Gst.MessageTypeError `elem` messageTypes) $ do
    (gerror,_debug) <- Gst.messageParseError message
    T.hPutStrLn stderr . ("Error: " <>) =<< Gst.gerrorMessage gerror
    T.hPutStrLn stderr _debug
    GLib.mainLoopQuit loop

  return True

onPadAdded :: Gst.Element -> Gst.Pad -> IO ()
onPadAdded decoder pad = do
  -- We can now link this pad with the vorbis-decoder sink pad
  putStrLn "Dynamic pad created, linking demuxer/decoder"
  Just sinkPad <- Gst.elementGetStaticPad decoder "sink"
  void $ Gst.padLink pad sinkPad


gstreamer :: [String] -> IO ()
gstreamer args = do

  -- Initialization
  void $ Gst.init Nothing
  let filename = head args

  loop <- GLib.mainLoopNew Nothing False


  -- Crate gstreamer elements
  pipeline <- Gst.pipelineNew (Just $ pack "video-player")

  let makeElement factoryname name =
        fromMaybe (error $ unpack $ pack "Could not create " <> name)
        <$> Gst.elementFactoryMake factoryname (Just name)

  -- gst-launch-1.0 filesrc location=/home/jun/Videos/1.mp4 ! decodebin3 ! videoconvert ! autovideosink
  source  <- makeElement (pack "filesrc") (pack "filesrc")
  decode  <- makeElement (pack "decodebin") (pack "decoder")
  conv    <- makeElement (pack "videoconvert") (pack "videoconvert")
  sink    <- makeElement (pack "autovideosink") (pack "autovideosink")

  -- Set up the pipeline

  mFactory <- elementGetFactory source
  case mFactory of
    Just factory -> do
      feature <- toPluginFeature factory
      pluginName <- pluginFeatureGetPluginName feature
      print $ fromMaybe "nope" pluginName
    Nothing -> do
      putStrLn "nothing"

  tracerFactorys <- tracerFactoryGetList
  mapM_ pluginFeatureLoad tracerFactorys

  tracerTypes <- mapM tracerFactoryGetTracerType tracerFactorys  
  types <- mapM gtypeName tracerTypes
  print types

  let (_:_:stats:_) = types



  pluginFeatureListFree tracerFactorys

  name <- getObjectPropertyString source "name"
  putStrLn $ unpack $ fromMaybe (pack "not found") name
  -- we set the input filename to the source element
  setObjectPropertyString source "location" (Just $ pack filename)

  -- we add a message handler
  bus <- Gst.pipelineGetBus pipeline
  busWatchId <- Gst.busAddWatch bus GLib.PRIORITY_DEFAULT (busCall loop)

  -- we add all elements into the pipeline
  -- file-source | ogg-demuxer | vorbis-decoder | converter | alsa-output
  mapM_ (Gst.binAdd pipeline) [source, decode, conv, sink]

  -- we link the elements together
  -- file-source -> ogg-demuxer ~> vorbis-decoder -> converter -> alsa-output
  void $ Gst.elementLink source decode
  -- void $ Gst.elementLink decode conv
  void $ Gst.elementLink conv sink

  -- let elementLinkMany (a:b:cs) = (&&) <$> Gst.elementLink a b <*> elementLinkMany (b:cs)
  --     elementLinkMany _ = return True

  -- void $ elementLinkMany [sink]

  void $ Gst.onElementPadAdded decode (onPadAdded conv)

  {- note that the demuxer will be linked to the decoder dynamically.
     The reason is that Ogg may contain various streams (for example
     audio and video). The source pad(s) will be created at run time,
     by the demuxer when it detects the amount and nature of streams.
     Therefore we connect a callback function which will be executed
     when the "pad-added" is emitted.-}

  -- Set the pipeline to "playing" state
  putStrLn $ "Now playing: " ++ filename
  void $ Gst.elementSetState pipeline Gst.StatePlaying

  -- Set periodic clock
  clock <- Gst.pipelineGetPipelineClock pipeline
  start <- Gst.clockGetTime clock
  afterTenSecond <- Gst.clockNewSingleShotId clock (start + 10000000000)

  let 
    switchLocation :: Gst.ClockCallback
    switchLocation _ _ _  = do
      print "Switch!"
      Gst.elementSetState pipeline Gst.StateReady  
      setObjectPropertyString source "location" (Just "/home/jun/Videos/1.mp4")
      Gst.elementSetState pipeline Gst.StatePlaying
      return True

  Gst.clockIdWaitAsync afterTenSecond switchLocation

  -- Iterate
  putStrLn "Running..."
  #run loop

  -- Out of the main loop, clean up nicely
  putStrLn "Returned, stopping playback"
  void $ Gst.elementSetState pipeline Gst.StateNull

  putStrLn "Deleting pipeline"
  -- void $ GLib.sourceRemove busWatchId