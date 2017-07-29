{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Errors2
(mapM3
) where
import Prelude hiding (undefined)
import Data.Either(lefts,rights)
import Control.Monad.Writer

-- similar to mapM but keeps track of all errors
mapM2 :: (d -> Either c b) -> [d] -> Either [c] [b]
mapM2 f ds = let{es = map f ds; (ls,rs) = (lefts es, rights es)} in
 case ls of
  [] -> Right rs
  _ -> Left ls

-- similar to mapM but keeps track of all warnings
mapM3 :: (Monoid e) => (d -> WriterT e (Either c) b) -> [d] -> WriterT e (Either [c]) [b]
mapM3 f ds = WriterT tmp where 
 tmp = do -- Either [c] ([b], e)
  bes <- mapM2 (runWriterT . f) ds -- Either [c] [(b,e)]
  return (map fst bes, mconcat $ map snd bes)
