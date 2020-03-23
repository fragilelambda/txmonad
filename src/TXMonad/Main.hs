{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TXMonad.Main
  ( txmonad
  )
where

import           Control.Monad                  ( when )
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Map                      as M
import           Data.Monoid                    ( getAll )
import           TXMonad.Config
import           TXMonad.Core
import           TXMonad.Operations
import           TXMonad.StackSet               ( new )

txmonad :: (LayoutClass l Window, Read (l Window)) => TXConfig l -> IO ()
txmonad = launch

launch :: (LayoutClass l Window, Read (l Window)) => TXConfig l -> IO ()
launch initxmc = do
  let xmc = initxmc { layoutHook = Layout $ layoutHook initxmc }
  let layout = layoutHook xmc
      initialWinset =
        let padToLen n xs = take (max n (length xs)) $ xs ++ repeat ""
        in  new layout (padToLen (length $ sd xmc) (workspaces xmc)) (sd xmc)
      cf = TXConf { config = xmc, keyActions = keys xmc xmc }
      st = TXState { windowset = initialWinset, uniqueCnt = 0 }
  runTX cf st $ do
    printScreen
    forever $ prehandle =<< io getLine
  return ()
  where prehandle e = handleWithHook e

handleWithHook :: Event -> TX ()
handleWithHook e = do
  evHook <- asks (handleEventHook . config)
  scHook <- asks (screenEventHook . config)
  whenTX (userCodeDef True $ getAll `fmap` evHook e) (handle e)
  whenTX (userCodeDef True $ getAll `fmap` scHook e) printScreen

handle :: Event -> TX ()
handle e = do
  ks <- asks keyActions
  userCodeDef () $ whenJust (M.lookup e ks) id
  return ()
