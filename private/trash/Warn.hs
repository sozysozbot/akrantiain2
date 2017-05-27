{-# OPTIONS -Wall #-}
module Akrantiain.Warn
(WarnLevel(..)
,Warning()
,Warnings
,warn
,pretty
,tellOne
)where
import Text.Parsec(SourcePos)
import Data.Sequence
import Control.Monad.Writer

data Warning = Warn String WarnLevel SourcePos

type Warnings = Seq Warning

tellOne :: (Monad m) => a -> WriterT (Seq a) m ()
tellOne = tell . singleton 

warn :: String -> WarnLevel -> SourcePos -> Warning
warn = Warn

pretty :: WarnLevel -> Warning -> String
pretty limit (Warn str level pos) 
 | limit > level = ""
 | otherwise     = "warning (" ++ p level ++ ") at " ++ show pos ++ ":\n" ++ str ++ "\n"
 where 
  p Crucial = "crucial"
  p Important = "important"
  p Helpful = "helpful"
  p Verbose = "verbose"


data WarnLevel = Verbose | Helpful | Important | Crucial deriving(Show,Eq,Ord)