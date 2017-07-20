module Akrantiain.SanitizeSentences
(sanitizeSentences
) where
import Prelude hiding (undefined)
import Akrantiain.Structure
import Akrantiain.Errors
import Akrantiain.Rule
import Control.Monad(unless)
import Data.List(group, sort)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Either(lefts, rights)
import Control.Arrow((&&&))

split3 :: [Sentence] -> ([Conversion],[Identifier],[Define])
split3 [] = ([],[],[])
split3 (Left'   c:xs) = let (cs,is,ds) = split3 xs in (c:cs,is,ds)
split3 (Middle' i:xs) = let (cs,is,ds) = split3 xs in (cs,i:is,ds)
split3 (Right'  d:xs) = let (cs,is,ds) = split3 xs in (cs,is,d:ds)


toSettingSpecifier' :: Identifier -> Either Identifier SettingSpecifier
toSettingSpecifier' i = case toSettingSpecifier i of
 Nothing -> Left i
 Just a -> Right a

sanitizeSentences :: [Sentence] -> SemanticMsg (Environment,[Conversion],M.Map Identifier (Choose Quote))
sanitizeSentences sents = do
 let (convs, vars_pre, defs_pre) = split3 sents
 let defs = map (\(Define a b) -> (a,b)) defs_pre
 let (unknowns, vars') = lefts &&& rights $ map toSettingSpecifier' vars_pre
 unless (null unknowns) $ tell [SemanticWarning{warnNum = 2435, warnStr = "unknown setting-specifier(s) " ++ toBraces unknowns}]
 let vars = S.fromList vars'
 let duplicates = (map head . filter (\x -> length x > 1) . group . sort . map fst) defs
 unless (null duplicates) $ lift $ Left E{errNum = 334, errStr = "duplicate definition regarding identifier(s) " ++ toBraces duplicates}
 let defs_ = M.fromList defs
 let punct = case Id "PUNCTUATION" `M.lookup` defs_ of{Nothing -> "";
  Just (Ch arr) -> arr >>= unQ} -- FIXME: THIS CONCAT ISN'T RIGHT (, though, at least it is explicitly explained in manual)
 return(Env{pun=punct, bools=vars},convs,defs_)
