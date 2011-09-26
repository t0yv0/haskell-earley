{-# OPTIONS -XDoRec -XDeriveDataTypeable #-}

module Example where

import Earley
import qualified Grammar as G
import Data.Typeable (Typeable)
import Control.Applicative

data E = Nat Int
       | Add E E deriving (Show, Typeable)

grammar :: G.Grammar (G.Rule Char E)
grammar = do
  nat <- G.rule "NAT"
         [ fmap (\_ -> 0) (G.term '0')
         , fmap (\_ -> 1) (G.term '1')
         ]
  rec expr <- G.rule "EXPR"
              [ fmap Nat $ G.var nat
              , pure (\x _ y -> Add x y)
                <*> G.var expr
                <*> G.term '+'
                <*> G.var expr
              ]
  return expr

-- See figure 2 in 
-- http://webhome.cs.uvic.ca/~nigelh/Publications/PracticalEarleyParsing.pdf
badG :: G.Grammar (G.Rule Char ())
badG = do
  e <- G.rule "E" [pure ()]
  a <- G.rule "A" [G.term 'a', G.var e]
  s <- G.rule "S" [pure (\_ _ _ _ -> ())
                   <*> G.var a
                   <*> G.var a
                   <*> G.var a
                   <*> G.var a]
  return s

test :: Show a => Parser t a -> [t] -> t -> IO ()
test (Parser n) [] eof = do
  (res, p) <- n eof
  case res of
    Just v -> do putStr "MATCH:"
                 putStrLn (show v)
    Nothing -> return ()
test (Parser n) (x:xs) eof = do
  (res, p) <- n x
  case res of
    Just v -> do putStr "MATCH:"
                 putStrLn (show v)
    Nothing -> return ()
  test p xs eof

tp :: (Show a, Typeable a) => G.Grammar (G.Rule Char a) -> String -> IO ()
tp p i =
  let pa = (parse p) in
  test pa i '\000'
