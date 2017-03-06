{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Expand
(SemanticError(..)
,expand
,Conv2(..)
,Orthography'(..)
) where

import Akrantiain.Structure
import Akrantiain.Resolve_definitions
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List(intercalate)
import Control.Monad(mapM, forM)

data Conv2 = Conv (Array Orthography') [Phoneme] deriving(Show, Eq, Ord)

-- List required only for $-numbering, which will be numbering [Orthography']
data Orthography' = Neg' [Resolved] | Pos' [Resolved] deriving(Show, Eq, Ord)

check_length :: Set Sentence -> Either SemanticError (Set Sentence)
check_length sents = case [ (orthos, phonemes) | Conversion orthos phonemes <- sents, (length[()|Pos _ <- orthos] - length[()|Pos(Res Boundary) <- orthos ]) /= length phonemes] of
 [] -> return sents
 arr  -> Left $ E{errNum = 3, errStr = "mismatched number of concrete terms in left- and right-hand side of "++ intercalate ", " ["{"++ toSource (Conversion os ps)++"}" | (os,ps) <- arr]}


expand :: Set Sentence -> Either SemanticError (Set Conv2)
expand sents' = do 
 sents <- check_length sents'
 (orthoset, identmap) <- split sents
 newMap <- candids_to_quotes identmap
 u <- forM (S.toList orthoset) $ \(phonemes, ortho_arr) -> do -- Either
  x1 <- f' newMap ortho_arr 
  return [Conv orthos' phonemes | orthos' <- x1]
 return $ concat u 



f' :: M.Map Identifier (Set Resolveds) -> Array Orthography -> Either SemanticError (Set (Array Orthography'))
f' identmap ortho_arr = do -- Either
 arr <- mapM g ortho_arr
 return $ sequence arr 
 where
  g :: Orthography -> Either SemanticError [Orthography']
  g (Neg c) = map (Neg' . unR) <$> h c 
  g (Pos c) = map (Pos' . unR) <$> h c
  h (Res reso) = Right [ R[reso] ]
  h (Ide ident@(Id i)) = case M.lookup ident identmap of 
   Nothing -> Left $ E{errNum = 4, errStr = "unresolved identifier {" ++ i ++ "}"}
   Just v  -> Right v

-- identmap: v --> [R ["a"], R ["o", "e"] ]
-- ortho_arr : [Pos "i", Pos v, Neg "r"]
-- result : [ [Pos' ["i"], Pos' ["a"], Neg' ["r"]], [Pos' ["i"], Pos' ["o", "e"], Neg' ["r"]] ]
-- f :: M.Map Identifier (Set Resolveds) -> Array Orthography -> Set (Array Orthography')
-- f identmap ortho_arr = sequence $ map g ortho_arr where
  -- g (Neg c) = map (Neg' . unR) $ h c
  -- g (Pos c) = map (Pos' . unR) $ h c
  -- h (Res reso) = [ R[reso] ]
  -- h (Ide ident) = fromJust $ M.lookup ident identmap
-- g: 
-- Pos "i" -> [ Pos' ["i"] ]
-- Neg v -> [ Neg' ["a"] , Neg' ["o", "e"] ]
--
-- map g ortho_arr:
-- [
--     [ Pos' ["i"] ],
--     [ Pos' ["a"] , Pos' ["o", "e"] ],
--     [ Neg' ["r"] ]
-- ]
-- 


split :: Set Sentence -> Either SemanticError (S.Set(Array Phoneme, Array Orthography),M.Map Identifier (Set Candidates))
split [] = Right (S.empty, M.empty)
split (Conversion orthos phonemes : xs) = do 
  (s,m) <- split xs 
  return (S.insert (phonemes, orthos) s , m) -- duplicate is detected later
split (Define ident@(Id i) cands : xs) = do 
  (s,m) <- split xs 
  if ident `M.member` m 
   then Left $ E{errNum = 0, errStr = "duplicate definition of identifier {"++ i ++ "}"} 
   else Right (s, M.insert ident cands m)

