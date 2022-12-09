module Utils.Tree (
  BTree (..),
  Context (..),
  Zipper,
  mkZipper,
  unZipper,
  up,
  down,
  topmost,
  insert,
  findChild,
  modify
) where

import Data.Bifunctor (first)
import Data.List (elemIndex, findIndex)

data BTree a = Node a [BTree a] | Leaf a
  deriving (Eq, Show, Functor)

data Context a = Root | Branch a [BTree a] [BTree a] (Context a)
  deriving (Eq, Show)

type Zipper a = (BTree a, Context a)

mkZipper :: BTree a -> Zipper a
mkZipper t = (t, Root)

unZipper :: Zipper a -> BTree a
unZipper = fst . topmost

up :: Zipper a -> Zipper a
up z@(_, Root) = z
up (tree, Branch x before after ctx) =
  (Node x (before ++ tree:after), ctx)
  
down :: Int -> Zipper a -> Zipper a
down _ z@(Leaf _, _) = z
down i (Node p children, ctx)
  | i < 0 || i >= length children = error "'i' out of range" 
  | otherwise =
    let (before, x:after) = splitAt i children in
      (x, Branch p before after ctx)

topmost :: Zipper a -> Zipper a
topmost z@(_, Root) = z
topmost z = topmost $ up z

insert :: BTree a -> Zipper a -> Zipper a
insert t = first (prependChild t)

prependChild :: BTree a -> BTree a -> BTree a
prependChild _ (Leaf _) = error "cannot append a child node to a Leaf"
prependChild child (Node x children) = Node x (child:children)

findChild :: (a -> Bool) -> Zipper a -> Maybe Int
findChild _ (Leaf _, _) = Nothing
findChild f (Node _ children, _) = findIndex f (value <$> children)

value :: BTree a -> a
value (Leaf x) = x
value (Node x _) = x

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (tree, ctx) = case tree of
  Node x children -> (Node (f x) children, ctx)
  Leaf x -> (Leaf (f x), ctx)
