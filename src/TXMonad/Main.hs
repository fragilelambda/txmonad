{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TXMonad.Main
  ( txmonad
  ) where

import           TXMonad.Core
import           TXMonad.StackSet (new)
import qualified TXMonad.StackSet as W

txmonad :: (LayoutClass l Window, Read (l Window)) => TXConfig l -> IO ()
txmonad conf = launch conf

launch :: (LayoutClass l Window, Read (l Window)) => TXConfig l -> IO ()
launch initxmc = do
  let xmc = initxmc {layoutHook = Layout $ layoutHook initxmc}
  let layout = layoutHook xmc
      initialWinset =
        let padToLen n xs = take (max n (length xs)) $ xs ++ repeat ""
         in new layout (padToLen (sd xmc) (workspaces xmc)) (sd xmc)
      cf = TXConf {config = xmc}
      st = TXState {windowset = initialWinset}
  return ()
