{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TXMonad.Main
  ( txmonad
  )
where

import           Control.Monad.Reader

import           TXMonad.Core
import           TXMonad.Config
import           TXMonad.StackSet               ( new )
import           Data.Monoid                    ( getAll )

txmonad :: (LayoutClass l Window, Read (l Window)) => TXConfig l -> IO ()
txmonad = launch

launch :: (LayoutClass l Window, Read (l Window)) => TXConfig l -> IO ()
launch initxmc = do
  let xmc = initxmc { layoutHook = Layout $ layoutHook initxmc }
  let layout = layoutHook xmc
      initialWinset =
        let padToLen n xs = take (max n (length xs)) $ xs ++ repeat ""
        in  new layout (padToLen (sd xmc) (workspaces xmc)) (sd xmc)
      cf = TXConf { config = xmc }
      st = TXState { windowset = initialWinset }
  runTX cf st $ do
    forever $ prehandle =<< io getLine
  return ()
  where prehandle e = handleWithHook e

handleWithHook :: Event -> TX ()
handleWithHook e = do
  evHook <- asks (handleEventHook . config)
  whenTX (userCodeDef True $ getAll `fmap` evHook e) (handle e)

handle :: Event -> TX ()
handle e = do
  io $ putStrLn e
  return ()
