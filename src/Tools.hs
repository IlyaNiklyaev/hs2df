module Tools where

import Data.Tree
import Data.Graph.Inductive
import Data.DList (singleton, fromList, toList)
import Control.Monad.RWS
import Control.Arrow

treeToGraph :: Tree a -> Gr a ()
treeToGraph t = uncurry mkGraph . (toList *** toList) . snd $ evalRWS (go t) () [1..]
  where go (Node a ns) = do
          i <- state $ head &&& tail
          es <- forM ns $ go >=> \j -> return (j, i, ())
          tell (singleton (i, a), fromList es)
          return i