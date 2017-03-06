{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Akrantiain.Cook
(cookBy
,Input
,Output
,Fixme3(..)
,RuntimeError(..)
) where
import Akrantiain.Structure
import Akrantiain.Errors


data Fixme3 = Fixme3 
data Fixme4 = Fixme4 
type Input = String
type Output = Either RuntimeError String
cookBy :: Fixme3 -> Either SemanticError( Input -> Output )
cookBy foobar = undefined
-- cookBy foobar = Right func -- ***FIXME***
 -- where 
  -- func x
   -- | x == "wa" = Right "わ"
   -- | x == "wu" = Left $ RE{errNo = 1030, errMsg = "wu is not good"}
   -- | otherwise = Right x

   
cookBy' :: Fixme4 -> Input -> Output
cookBy' fixme4 input = undefined


data Stat = Stat

type Rule = Array (Array Resolved, Array Phoneme)

update :: Rule -> Stat -> Stat
update = undefined













