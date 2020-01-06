module Day06.Lib where

import           Lib

type EdgeList = [Edge]

type NodeName = String

data Node =
  Node NodeName [Node]
  deriving (Show)

instance Eq Node where
  (Node name1 _) == (Node name2 _) = name1 == name2

data Edge =
  Edge Node Node
  deriving (Show)

instance Read Edge where
  readsPrec _ str = [(Edge (Node a []) (Node b []), "")]
    where
      [a, b] = splitOn ')' str

nodeList :: EdgeList -> [Node]
nodeList = list' []
  where
    list' stored [] = stored
    list' stored (Edge a b:xs) = list' stored' xs
      where
        stored' = [a | a `notElem` stored] ++ [b | b `notElem` stored] ++ stored

childrenOf :: Node -> [Node]
childrenOf (Node _ x) = x

allSource :: EdgeList -> [Node]
allSource []            = []
allSource (Edge a _:xs) = a : allSource xs

allTarget :: EdgeList -> [Node]
allTarget []            = []
allTarget (Edge _ b:xs) = b : allTarget xs

nodesWithoutParent :: EdgeList -> [Node]
nodesWithoutParent list = filter (`notElem` targets) sources
  where
    sources = allSource list
    targets = allTarget list

buildTreeFrom :: EdgeList -> Node -> Node
buildTreeFrom list (Node name _) = Node name children
  where
    children = map (buildTreeFrom list . getTarget) $ filter filter' list
      where
        filter' (Edge (Node name' _) _) = name == name'
    getTarget (Edge _ node) = node

buildPathTo :: Node -> NodeName -> [NodeName]
buildPathTo (Node name ch) target
  | name == target = [name]
  | null ch = []
  | otherwise = name : options'
  where
    options = (filter (elem target) . map (`buildPathTo` target)) ch
    options' =
      if null options
        then []
        else head options

data Lists a
  = List [a]
  | ListOfLists [Lists a]

flatten :: Lists a -> [a]
flatten (List xs)         = xs
flatten (ListOfLists xss) = concatMap flatten xss
