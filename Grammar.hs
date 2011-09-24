{-# OPTIONS -XDoRec -XDeriveDataTypeable #-}

module Grammar (Element(..), Grammar, Part(..), Rule(..),
                rule, scan, term, var,
                showGrammar, printGrammar, runGrammar) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.Dynamic
import qualified Data.IntMap as IM
--import qualified Data.IntSet as IS
import qualified Thread as T

type Grammar a = State (RuleID, IM.IntMap Clause) a

data Element t = Term Name (t -> Bool)
               | Var RuleID [([Element t], T.Thread Dynamic)]

data Atom = NonTerminal RuleID
          | Terminal Name

type Clause = (Name, [[Atom]])

type Name = String

type RuleID = Int

data Rule t a = Rule RuleID Name [Part t a]

data Part t a = Part [Element t] (T.Thread a)

instance Functor (Part t) where
  fmap f (Part e p) = Part e (fmap f p)

instance Applicative (Part t) where
  pure x = Part [] (pure x)
  Part a x <*> Part b y = Part (a ++ b) (x <*> y)

showGrammar :: Grammar a -> String
showGrammar g = res where
  res = unlines $ concat ls
  ls = [(n ++ " ::") : [showLine e | e <- es] | (n, es) <- clauses]
  showLine es = unwords $ "" : [showElement e | e <- es]
  showElement (NonTerminal i) = fst ((IM.!) m i)
  showElement (Terminal n) = n
  m = snd $ snd $ runState g (0, IM.empty)
  clauses = IM.elems m

printGrammar :: Grammar a -> IO ()
printGrammar = putStrLn . showGrammar

runGrammar :: Grammar a -> a
runGrammar g = fst $ runState g (0, IM.empty)

-- getNullRuleIDs :: Grammar (Rule t a) -> IS.IntSet
-- getNullRuleIDs g = IS.fromList [i | i <- IM.keys rules, isN IS.empty i] where
--   isN s k = any (all ok) $ at k where
--     s' = IS.insert k s
--     ok (NonTerminal j) = IS.member j s'
--     ok (Terminal _) = False
--   at x = snd $ (IM.!) rules x
--   (_, (_, rules)) = runState g (0, IM.empty)

rule :: String -> [Part t a] -> Grammar (Rule t a)
rule name parts = do
  (i, m) <- get
  let ps = [[conv e | e <- es] | Part es _ <- parts]
      conv (Term n _) = Terminal n
      conv (Var j _) = NonTerminal j
  put (i + 1, IM.insert i (name, ps) m)
  return $ Rule i name parts

var :: (Typeable a, Typeable t) => Rule t a -> Part t a
var (Rule k _ ps) = Part es T.expect where
  es = [Var k [(e, fmap toDyn s) | Part e s <- ps]]

scan :: Typeable t => Name -> (t -> Bool) -> Part t t
scan n f = Part [Term n f] T.expect 
      
term :: (Eq t, Show t, Typeable t) => t -> Part t ()
term t = fmap (const ()) (scan (show t) (== t))

