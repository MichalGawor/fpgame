{-# LANGUAGE OverloadedStrings #-}
module StateMachinesWithAdts where

import Control.Monad      (foldM)
import Data.List.NonEmpty
import Data.Text          (Text)
import Text.Printf        (printf)

data Actions = Return | Option1 | Option2 | Option3 | Option4
data CheckoutState = Menu | Lost | Playing | Paused deriving (Eq, Show)
type StateMachine s e = s -> e -> IO s

