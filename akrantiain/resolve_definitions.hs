{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Resolve_definitions
(SemanticError(..)
,candids_to_quotes
) where

import Akrantiain.Structure
-- import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe(fromJust)
import Prelude hiding (undefined)
data SemanticError = E {errNum :: Int, errStr :: String} deriving(Eq, Ord)
instance Show SemanticError where
 show E{errNum = n, errStr = str} = "Semantic error (error code #" ++ show n ++ ")\n" ++ str 

 
{-
candids_to_quotes

input:
vowel = "a" | "i"; conson = "b" | ^ "c"; syll = conson vowel | vowel

output:
vowel = "a" | "i"; conson = "b" | ^ "c"; syll = "b" "a" | ^ "c" "a" | "b" "i" | ^ "c" "i" | "a" | "i"


-}

type Stack = [Identifier]
type Temp = (M.Map Identifier [Candidates], M.Map Identifier [Resolveds], Stack) 

-- move identifier from cand_map to quot_map
reduce_1 :: Identifier -> Temp -> Either SemanticError Temp
reduce_1 ident@(Id i) temp@(cand_map, quot_map, stack) = case M.lookup ident cand_map of
 Nothing -> case M.lookup ident quot_map of 
  Nothing -> Left $ E{errNum = 1, errStr = "unresolved identifier {" ++ i ++ "}"}
  Just _ -> return temp
 Just candids_list -> if ident `elem` stack 
  then Left $ E{errNum = 2, errStr = "recursive definition regarding identifier {" ++ i ++ "}"} 
  else do
   let ide_list = [ ide | C candids <- candids_list, Ide ide <- candids]
   case ide_list of 
    [] -> do
     let resos_list = [ R[res | Res res <- candids] | C candids <- candids_list]
     return (M.delete ident cand_map, M.insert ident resos_list quot_map, stack)
    (x:_) -> do -- move x and resolve x
     (cand_map', quot_map', _) <- reduce_1 x (cand_map, quot_map, ident:stack) -- move x; x cannot use ident
     let resos_list = fromJust(x `M.lookup` quot_map') -- x is moved to quot_map'
     let candids_list' = replace_candids_list x resos_list candids_list -- replace (x --> resos_list) within ident's candids_list
     let cand_map'' = M.insert ident candids_list' cand_map' -- update ident's info
     reduce_1 ident (cand_map'', quot_map', stack) -- recursion; x is moved

replace_candids_list :: Identifier -> [Resolveds] -> [Candidates] -> [Candidates]
replace_candids_list x resos_list candids_list = 
 let {k = [ map Res reso_list | R reso_list <- resos_list];
      f u = [ C $ replace (Ide x) k' u | k' <- k]} in
  concat [ f candids | C candids <- candids_list] -- candids :: [Candidate]
-- Ide vowel -> [ R["a"], R["i", "u"] ] -> [C [conson, vowel] ,C [vowel]] -> [C[conson, "a"], C[conson, "i", "u"], C["a"], C["i","u"]]
-- k = [["a"], ["i","u"]]
-- f [conson, vowel] = [C[conson, "a"], C[conson, "i", "u"]]

replace :: (Eq a) => a -> [a] -> [a] -> [a]
replace before after = concatMap f where
 f u
  | u == before = after
  | otherwise = [u]
 
-- data Candidate = Res Resolved | Ide Identifier 
--  let f k = if k == Ide x then resos_list else 







candids_to_quotes :: M.Map Identifier [Candidates] -> Either SemanticError (M.Map Identifier [Resolveds])
candids_to_quotes old_map = c_to_q2 (old_map, M.empty)


c_to_q2 :: (M.Map Identifier [Candidates], M.Map Identifier [Resolveds]) -> Either SemanticError (M.Map Identifier [Resolveds])
c_to_q2 (cand_map, quot_map) = case M.lookupGE (Id "") cand_map of 
 Nothing -> return quot_map -- Any identifier is greater than (Id ""); if none, the cand_map must be empty
 Just (ident, _) -> do
  (cand_map', quot_map', _) <- reduce_1 ident (cand_map, quot_map, [])
  c_to_q2 (cand_map', quot_map')
