module Earley where

import Data.Dynamic
import qualified Grammar as G
import qualified Thread as T
import qualified Data.Set as S
import qualified Data.IntMap as IM

data Key = Key !RuleID !Branch !Offset !SetID deriving (Eq, Ord, Show)
type RuleID = Int
type SetID = Int
type Branch = Int
type Offset = Int

data Item t = Item !Key [G.Element t] (S.Set (Item t)) (T.Thread Dynamic)

instance Eq (Item t) where
  Item a _ _ _ == Item b _ _ _ = a == b

instance Ord (Item t) where
  compare (Item a _ _ _) (Item b _ _ _) = compare a b

instance Show (Item t) where
  show (Item k e _ _) =
    show k ++ "; LEFT: " ++ show (length e)

advance :: String -> Dynamic -> Item t -> Item t
advance _ x (Item a (_:es) s st) = Item (incr a) es s (T.feed x st) where
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
        val = T.force st
        ns' = IM.insert a val ns
        find set = [ advance ("C" ++ show i ++ show j) val it
                   | it@(Item _ (G.Var b _:_) _ _) <- S.toList set, a == b ]
    match (Item _ (G.Var b el:_) _ _) = res where
      res = re s0x s1 (is $ IM.lookup b ns)      
      is (Just d) = advance "P" d x : items
      is Nothing = items      
      items = [ Item (Key b j 0 i) es sc st
              | (j, (es, st)) <- zip [0..] el ] ++ xs
    match (Item _ (G.Term _ ok:_) _ _)
      | ok t = re s0x (S.insert (advance "S" (toDyn t) x) s1) xs
      | otherwise = re s0x s1 xs

data Parser t a = Parser (t -> IO (Maybe a, Parser t a))

parse :: (Typeable t, Typeable a) => G.Grammar (G.Rule t a) -> Parser t a
parse gr = loop 0 s0 where
  g = do start <- gr
         dummy <- G.rule "START" [G.var start]
         return dummy
  G.Rule i _ [G.Part es st] = G.runGrammar g
  s0 = S.fromList [Item (Key i 0 0 0) es s0 st0]
  end = Item (Key i 0 1 0) [] s0 st0
  st0 = fmap toDyn st
  eval s 
    | S.member end s = Just value 
    | otherwise = Nothing
    where
      force x = fromDyn (T.force x) (error "Failed to downcast.")
      value = head [force x | it@(Item _ _ _ x) <- S.toList s, it == end]
  loop j s = Parser $ \t ->
    let (sc, sn) = earley j t IM.empty sc S.empty S.empty (S.toList s)
        res = eval sc
        p = loop (j + 1) sn in
    do -- putStrLn $ "#" ++ show j ++ ": " 
       -- putStr "  "
       -- putStrLn (show s)
       -- putStr "  "
       -- putStrLn (show sc)
       -- putStr "  "
       -- putStrLn (show sn)
       return (res, p)

