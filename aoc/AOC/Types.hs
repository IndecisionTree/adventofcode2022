{-# language GADTs #-}
module AOC.Types (
  Solution (..),
  Submission (..)
) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Web.Internal.FormUrlEncoded (ToForm)
import Type.Reflection

data Solution where
  Solution :: (Typeable b, Show b) =>
    (Text -> a) -> (a -> b) -> (a -> b) -> Solution

data Submission = Submission {
  part :: Int,
  answer :: String
} deriving (Generic)

instance ToForm Submission
