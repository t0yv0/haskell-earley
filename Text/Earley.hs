{-# OPTIONS -XExistentialQuantification -XGeneralizedNewtypeDeriving #-}

module Text.Earley (
  Grammar, Rule, Parser(..), rule, term, var, parse, test
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.Dynamic
import qualified Data.IntMap as IM
import qualified Data.Set as S

data Thread a = Available !a
              | forall b. Typeable b => Expecting (b -> Thread a)

instance Functor Thread where
  fmap f (Available a) = Available (f a)
  fmap f (Expecting g) = Expecting (fmap f . g)

instance Applicative Thread where
  pure = Available
  Available a <*> b = fmap a b
  Expecting a <*> b = Expecting  $ \d -> a d <*> b

expect :: Typeable t => Thread t
expect = Expecting Available

feed :: Dynamic -> Thread a -> Thread a
feed _ (Available _) = error "Cannot feed a completed thread."
feed d (Expecting f) = f (fromDyn d err) where
  err = error "Failed to feed a thread: wrong argument type."

force :: Thread a -> a
force (Available x) = x
force _ = error "Cannot force a thread that is in progress."

type GS = (RuleID, IM.IntMap Clause)

newtype Grammar a =
  Grammar (State GS a)
  deriving (Monad, MonadFix, MonadState GS)

data Element t = Term Name (t -> Bool)
               | Var RuleID [([Element t], Thread Dynamic)]

data Atom = NonTerminal RuleID
          | Terminal Name

type Clause = (Name, [[Atom]])

type Name = String

type RuleID = Int

data Rule t a = Rule RuleID Name [Part t a]

data Part t a = Part [Element t] (Thread a)

instance Functor (Part t) where
  fmap f (Part e p) = Part e (fmap f p)

instance Applicative (Part t) where
  pure x = Part [] (pure x)
  Part a x <*> Part b y = Part (a ++ b) (x <*> y)

instance Show (Grammar a) where
  show x = showGrammar x

showGrammar :: Grammar a -> String
showGrammar (Grammar g) = res where
  res = unlines $ concat ls
  ls = [(n ++ " ::") : [showLine e | e <- es] | (n, es) <- clauses]
  showLine es = unwords $ "" : [showElement e | e <- es]
  showElement (NonTerminal i) = fst ((IM.!) m i)
  showElement (Terminal n) = n
  m = snd $ snd $ runState g (0, IM.empty)
  clauses = IM.elems m

runGrammar :: Grammar a -> a
runGrammar (Grammar g) = fst $ runState g (0, IM.empty)

rule :: String -> [Part t a] -> Grammar (Rule t a)
rule name parts = do
  (i, m) <- get
  let ps = [[conv e | e <- es] | Part es _ <- parts]
      conv (Term n _) = Terminal n
      conv (Var j _) = NonTerminal j
  put (i + 1, IM.insert i (name, ps) m)
  return $ Rule i name parts

var :: (Typeable a, Typeable t) => Rule t a -> Part t a
var (Rule k _ ps) = Part es expect where
  es = [Var k [(e, fmap toDyn s) | Part e s <- ps]]

scan :: Typeable t => Name -> (t -> Bool) -> Part t t
scan n f = Part [Term n f] expect

term :: (Eq t, Show t, Typeable t) => t -> Part t ()
term t = fmap (const ()) (scan (show t) (== t))

data Key = Key !RuleID !Branch !Offset !SetID deriving (Eq, Ord, Show)
type SetID = Int
type Branch = Int
type Offset = Int

data Item t = Item !Key [Element t] (S.Set (Item t)) (Thread Dynamic)

instance Eq (Item t) where
  Item a _ _ _ == Item b _ _ _ = a == b

instance Ord (Item t) where
  compare (Item a _ _ _) (Item b _ _ _) = compare a b

instance Show (Item t) where
  show (Item k e _ _) =
    show k ++ "; LEFT: " ++ show (length e)

advance :: String -> Dynamic -> Item t -> Item t
advance _ x (Item a (_:es) s st) = Item (incr a) es s (feed x st) where
  incr (Key i1 i2 i3 i4) = Key i1 i2 (i3 + 1) i4
advance s _ item =
  error ("Failed to advance an item: " ++ s ++ " " ++ show item)

type I t = Item t
type S t = S.Set (Item t)
type NS = IM.IntMap Dynamic

earley :: Typeable t => SetID -> t -> NS ->
          S t -> S t -> S t -> [I t] -> (S t, S t)
earley _ _ _ _ s0 s1 [] = (s0, s1)
earley i t ns sc s0 s1 (x:xs)
  | S.member x s0 = re s0 s1 xs
  | otherwise = match x
  where
    re = earley i t ns sc
    s0x = S.insert x s0
    match (Item (Key a _ _ j) [] sj st)
      | i == j = earley i t ns' sc s0x s1 (find s0 ++ xs)
      | otherwise = re s0x s1 (find sj ++ xs)
      where
        val = force st
        ns' = IM.insert a val ns
        find set = [ advance ("C" ++ show i ++ show j) val it
                   | it@(Item _ (Var b _:_) _ _) <- S.toList set, a == b ]
    match (Item _ (Var b el:_) _ _) = res where
      res = re s0x s1 (is $ IM.lookup b ns)
      is (Just d) = advance "P" d x : items
      is Nothing = items
      items = [ Item (Key b j 0 i) es sc st
              | (j, (es, st)) <- zip [0..] el ] ++ xs
    match (Item _ (Term _ ok:_) _ _)
      | ok t = re s0x (S.insert (advance "S" (toDyn t) x) s1) xs
      | otherwise = re s0x s1 xs

data Parser t a = Parser (Maybe a) (t -> Parser t a)

parse :: (Typeable t, Typeable a) => Grammar (Rule t a) -> Parser t a
parse gr = loop Nothing 0 s0 where
  g = do start <- gr
         dummy <- rule "START" [var start]
         return dummy
  Rule i _ [Part es st] = runGrammar g
  s0 = S.fromList [Item (Key i 0 0 0) es s0 st0]
  end = Item (Key i 0 1 0) [] s0 st0
  st0 = fmap toDyn st
  eval s
    | S.member end s = Just value
    | otherwise = Nothing
    where
      ev x = fromDyn (force x) (error "Failed to downcast.")
      value = head [ev x | it@(Item _ _ _ x) <- S.toList s, it == end]
  loop r j s = Parser r $ \t ->
    let (sc, sn) = earley j t IM.empty sc S.empty S.empty (S.toList s)
        res = eval sc in
    loop res (j + 1) sn

test :: (Show a, Typeable a) => Grammar (Rule Char a) -> String -> IO ()
test gr input = loop (parse gr) input where
  loop (Parser result next) tokens = report result >> match tokens next
  match (x:xs) next = loop (next x) xs
  match _ next = end (next '\0')
  end (Parser result _) = report result
  report (Just x) = putStr "MATCH: " >> putStrLn (show x)
  report Nothing = return ()
