{-# language ScopedTypeVariables, GADTs #-}

module AOC (
  module AOC.Types,
  mkAocClient,
  showSolution,
) where

import AOC.Types
import AOC.API
import Type.Reflection

-- TODO find a better way to avoid 'show'ing Stringlike things in
-- quotes ("") without resorting to end-users having to wrap Solutions
-- with something like data StringOr = StringLike a | NotStringLike b
showSolution :: forall a. (Typeable a, Show a) => a -> String
showSolution a = case eqTypeRep (typeRep @a) (typeRep @String) of
  Just HRefl -> a
  Nothing -> show a
