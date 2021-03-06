{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Akrantiain.Pattern_match
(cook
) where
-- import Prelude hiding (undefined)
import Data.Maybe(mapMaybe, isNothing, catMaybes)
import Data.List(isPrefixOf, isSuffixOf, inits, tails, intercalate, unfoldr)
import Data.Char(toLower)
import Data.Either(lefts, rights)
import Control.Monad(guard, (>=>))
import Akrantiain.Errors
import Akrantiain.Rule
import Akrantiain.Structure(Choose(..))
import Akrantiain.NFD
import qualified Data.Set as S
import Control.Arrow(first)
import Control.Monad.Reader
import Akrantiain.RevList
import Data.Foldable(toList) -- used for RevList

data Environment' = Wrap{sensitivity :: Bool, getEnv :: Environment} deriving(Ord,Eq,Show)


type StatElem = (String, Maybe String)
type Stat = [StatElem]
type StatPair = (RevList StatElem, Stat)

resolvePunctuation :: Environment -> StatElem -> Either String String
resolvePunctuation _ (_, Just b) = Right b
resolvePunctuation env (a, Nothing)
 | isSpPunct (pun env) a = Right " "
 | FALL_THROUGH `S.member` bools env = Right a 
 | otherwise = Left a

insensitive :: Rule -> Rule
insensitive R{leftneg=l, leftdollar=ld, middle=m, rightdollar=rd, rightneg=r} 
 = R{leftneg=fmap f l, leftdollar=map h ld, middle=map(first h<$>) m, rightdollar=map h rd, rightneg=fmap f r} where
 f :: Condition -> Condition
 f (Negation c) = Negation $ h c
 f NegBoundary = NegBoundary
 h :: Choose String -> Choose String
 h (Ch arr) = Ch . map (map toLower) $ arr
-- R{leftneg :: Maybe(Condition), middle :: [ Either Boundary_ (Choose String, W)], rightneg :: Maybe(Condition)}

convertAndSplit :: (String -> [t]) -> String -> [([t], Maybe a)]
convertAndSplit f str = map (\x -> ([x], Nothing)) $ f (" " ++ str ++ " ") -- extra spaces required for handling word boundary

cook :: Rules -> String -> Either RuntimeError String
cook (env,rls'') str_ = do
 let rls' = if S.member USE_NFD (bools env) then map applyNfds rls'' else rls''
 let str = if S.member USE_NFD (bools env) then nfd str_ else str_
 let (sensitive_match,rls,stat) 
      | CASE_SENSITIVE `S.member` bools env = (True, rls', convertAndSplit id str)
      | PRESERVE_CASE `S.member` bools env = (False, rls', convertAndSplit id str)
      | otherwise = (False, map insensitive rls', convertAndSplit (map toLower) str)
 let cooked = cook' rls stat `runReader` Wrap sensitive_match env
 let eitherList = map (resolvePunctuation env) cooked
 case lefts eitherList of
  [] -> do
   let ans = dropTwo $ concat $ rights eitherList
   if USE_NFD `S.member` bools env then return $ nfc ans else return ans
  strs -> do
   let msg = "{" ++ intercalate "}, {" strs ++ "}"
   Left RE{errNo = 210, errMsg = "no rules that can handle character(s) "++ msg} -- FIXME: better message that lets the user know which `r` made akrantiain crash

dropTwo :: String -> String
dropTwo = dropOne . reverse . dropOne . reverse
 where dropOne = \(' ':xs) -> xs -- GUARANTEED TO BE SAFE

cook' :: [Rule] -> Stat -> Reader Environment' Stat
cook' rls stat = foldM apply stat rls

-- merge is allowed, split is not
apply :: Stat -> Rule -> Reader Environment' Stat
apply stat rule = do
 frontback_array <- match rule stat
 case frontback_array of
  [] -> return stat
  c -> let (a,b) = last c in
   if toList a == stat then undefined else do
    newStat <- apply (toList a) rule
    return $ newStat ++ b



-- let g a = if even a then Just(a `div` 2) else Nothing
-- iterate' g 10000 == [10000,5000,2500,1250,625]
iterate' :: (a -> Maybe a) -> a -> [a]
iterate' f x = x : unfoldr (f >=> \u -> return(u,u)) x


-- cutlist [1,2,3] = [(Reverse[],[1,2,3]),(Reverse[1],[2,3]),(Reverse[2,1],[3]),(Reverse[3,2,1],[])]
cutlist :: [a] -> [(RevList a,[a])]
cutlist xs = iterate' move_one (Reverse[],xs)
 where
  move_one :: (RevList a,[a]) -> Maybe(RevList a,[a])
  move_one (_,[]) = Nothing
  move_one (Reverse bs,c:cs) = Just(Reverse(c:bs),cs)

rev2 ::  [([a], t)] -> [([a], t)]
rev2 = map (first reverse) . reverse

rev2_ ::  [([a], t)] -> RevList ([a], t)
rev2_ = fmap (first reverse) . Reverse

rev2__ ::  RevList([a], t) -> [([a], t)]
rev2__ = map (first reverse) . unReverse

upgrade :: ([a] -> Bool) -> ([a] -> Bool)
upgrade f str = all f $ tail $ inits str

upgrade2 :: ([a] -> Bool) -> ([a] -> Bool)
upgrade2 f str = all f $ init $ tails str

unCond :: Condition -> (Punctuation -> String -> Bool)
unCond (Negation c) = \_ -> no c
unCond NegBoundary = \punct str -> not(isSpPunct punct str)


handleBoundary :: Environment -> StatPair -> Maybe StatPair
handleBoundary env (front, back) = do
  let front' = unReverse front
  let punct = pun env
  let tmp = map(isSpPunct punct) $ filter (/="") $ map fst front'
  guard $ null tmp || head tmp
  let (b', f'') = span (isSpPunct punct . fst) front'
  return (Reverse f'', reverse b' ++ back)

-- check if the right-hand side can be analyzed as if it has *already* passed thru the rightdollar and cond
fooFilter :: (Environment',[Choose String],Maybe Condition) -> StatPair -> Bool
fooFilter (env',arr,cond) (_,b) = newFunc arr rightstr
 where
  sensitive = sensitivity env'
  p = (pun . getEnv) env'
  newFunc :: [Choose String] -> String -> Bool
  newFunc [] str = case cond of 
   Nothing -> True
   Just condition -> upgrade (unCond condition p) str
  newFunc (Ch x:xs) str = or [newFunc xs newStr | newStr <- catMaybes [droppingPrefix2 sensitive u str | u <- x]]
  rightstr = concatMap fst b :: String

match :: Rule -> Stat -> Reader Environment' [StatPair]

match R{leftneg=Nothing, leftdollar=[], middle=[], rightdollar = arr, rightneg=cond} stat = do
 env2 <- ask
 return $ filter (fooFilter (env2,arr,cond)) $ cutlist stat


match k@R{leftneg=Nothing, leftdollar=[], middle=Right(Ch pats,w):xs} stat =  do
 sensitive <- sensitivity <$> ask
 newMatch <- match k{middle=xs} stat
 return $ catMaybes [testPattern sensitive w fb pat | fb <- newMatch, pat <- pats]
match k@R{leftneg=Nothing, leftdollar=[], middle=Left():xs} stat = do
 newMatch <- match k{middle=xs} stat
 env <- getEnv <$> ask
 return $ mapMaybe (handleBoundary env) newMatch

match k@R{leftneg=cond, leftdollar=arr} stat = do
 env2 <- ask
 newMatch <- match k{leftneg=Nothing, leftdollar=[]} stat
 return $ filter (fooFilter2 (env2,cond,arr)) newMatch

-- check if the left-hand side can be analyzed as if it *would* (in the future) passed thru the rightdollar and cond
fooFilter2  :: (Environment',Maybe Condition,[Choose String]) -> StatPair -> Bool
fooFilter2 (env',cond,arr) (a,_) = newFunc2 (reverse arr) leftstr
 where
  sensitive = sensitivity env'
  p = (pun . getEnv) env'
  newFunc2 :: [Choose String] -> String -> Bool
  newFunc2 [] str = case cond of 
   Nothing -> True
   Just condition -> upgrade2 (unCond condition p) str
  newFunc2 (Ch x:xs) str = or [newFunc2 xs newStr | newStr <- catMaybes [droppingSuffix2 sensitive u str | u <- x]]
  leftstr = concatMap fst (toList a) :: String


testPattern :: Bool -> W -> StatPair -> String -> Maybe StatPair
testPattern sensitive w (front, back) pat = do
 let front' = rev2__ front
 let pat' = reverse pat
 taken <- takeTill sensitive pat' front'
 let taken' = rev2 taken
 case w of
  W w' -> do
   guard $ all (isNothing . snd) taken'
   return (rev2_ $ drop(length taken')front', (pat,Just w') : back)
  Dollar_ -> return (rev2_ $ drop(length taken')front', taken' ++ back)

takeTill :: Bool -> String -> Stat -> Maybe Stat
takeTill _ [] _ = Just []
takeTill _ _ [] = Nothing
takeTill sensitive str (x@(s,_):xs)
 | isPrefixOf2 sensitive s str = (x:) <$> takeTill sensitive (drop(length s)str) xs
 | otherwise = Nothing

-- bool: true if case sensitive
isPrefixOf2 :: Bool -> String -> String -> Bool
isPrefixOf2 True  = isPrefixOf
isPrefixOf2 False = \a b -> map toLower a `isPrefixOf` map toLower b

-- bool: true if case sensitive
isSuffixOf2 :: Bool -> String -> String -> Bool
isSuffixOf2 True  = isSuffixOf
isSuffixOf2 False = \a b -> map toLower a `isSuffixOf` map toLower b

-- drop `a` from `b` if `a` isPrefixOf2 `b`
droppingPrefix2 :: Bool -> String -> String -> Maybe String
droppingPrefix2 sens a b
 | (isPrefixOf2 sens) a b = Just(drop (length a)b)
 | otherwise = Nothing

-- drop `a` from `b` if `a` isSuffixOf2 `b`
droppingSuffix2 :: Bool -> String -> String -> Maybe String
droppingSuffix2 sens a b
 | (isSuffixOf2 sens) a b = Just . reverse . drop(length a) . reverse $ b
 | otherwise = Nothing
