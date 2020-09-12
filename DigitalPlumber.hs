{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GADTs, StandaloneDeriving, UndecidableInstances #-}

module DigPlum where

import Control.Arrow
import Data.Map.Strict (Map, (!))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.Functor.Compose
import Control.Monad.State

newtype Fix f = In { out :: f (Fix f) }

deriving instance Show (f (Fix f))  ⇒ Show (Fix f)

type Algebra f a = f a -> a
cata :: (Functor f) => Algebra f a -> Fix f -> a  
cata f = out >>> fmap (cata f) >>> f
-- cata :: (Base t a -> a) -> t -> a

type Coalgebra f a = a -> f a
ana :: (Functor f) => Coalgebra f a -> a -> Fix f  
ana f = In <<< fmap (ana f) <<< f


type RAlgebra f a = f (Fix f, a) -> a
para :: Functor f => RAlgebra f a -> Fix f -> a  
para f = out >>> fmap (id &&& para f) >>> f

-- para :: (Base t (t, a) -> a) -> t -> a

type CVAlgebra f a = f (Attr f a) -> a
histo :: Functor f => CVAlgebra f a -> Fix f -> a  
histo h = worker >>> attribute where  
  worker = out >>> fmap worker >>> (h &&& id) >>> mkAttr
  mkAttr (a, b) = Attr a b

--  Recursive t => (Base t (Cofree (Base t) a) -> a) -> t -> a


data Attr f a = Attr  
            { attribute :: a
              , hole      :: f (Attr f a)
            }



data PipeTree a =
     Node Int [a]
     deriving (Eq,Show,Functor, Foldable, Traversable)


ident :: PipeTree a -> Int
ident (Node n _) = n

type GoodTree = Fix PipeTree

input = readFile "./test/Golden/DigPlum.txt"

sample = "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5"

bmap f f' (x,y) = (f x, f' y)

handleLine :: String -> (Int, [Int])
handleLine =  bmap read (map read. splitOn ", ") . fmap (fromJust . stripPrefix " <-> ") .  span (/=' ')
                   
toMap :: String -> Map Int [Int]
toMap = M.fromList . map handleLine . lines

getPair :: Ord k => Map k v -> k -> (k,v)
getPair m key = (key,  m ! key)

runStateTree ∷ Fix (Compose (State (Set Int)) PipeTree) → GoodTree
runStateTree = flip evalState S.empty . cata f
  where
    f ∷ Algebra (Compose (State (Set Int)) PipeTree) (State (Set Int) GoodTree)
    f x = fmap In $ getCompose x >>= sequence

makeGraph :: Map Int [Int] -> (Int -> GoodTree)
makeGraph master n = runStateTree $ makeGraph' master n

makeGraph' :: Map Int [Int] -> Int -> Fix (Compose (State (Set Int)) PipeTree)
makeGraph' master = ana fictional
    where fictional :: Coalgebra (Compose (State (Set Int) )PipeTree) Int
          fictional k = let (z,zs) = getPair master k in Compose $ do
            visitedNodes ← get
            let zs' = if k `S.member` visitedNodes then [ ] else zs
            put (S.insert k visitedNodes)
            return $ Node z zs'
 
 
     
getConnected :: GoodTree -> Set Int
getConnected = histo cvalg
    where cvalg :: CVAlgebra PipeTree (Set Int)
          cvalg (Node n xs) = S.singleton n `S.union` (S.unions . map attribute . filter (not . isPresent) $ xs)
          isPresent :: Attr PipeTree (Set Int) -> Bool
          isPresent n =  S.member (ident . hole $ n) (attribute n)