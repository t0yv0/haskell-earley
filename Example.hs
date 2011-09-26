{-# OPTIONS -XDoRec -XDeriveDataTypeable #-}

module Example where

import Text.Earley
import Data.Dynamic
import Control.Applicative

data E = Nat Int
       | Add E E deriving (Show, Typeable)

grammar :: Grammar (Rule Char E)
grammar = do
  nat <- rule "NAT"
         [ fmap (\_ -> 0) (term '0')
         , fmap (\_ -> 1) (term '1')
         ]
  rec expr <- rule "EXPR"
              [ fmap Nat $ var nat
              , pure (\x _ y -> Add x y)
                <*> var expr
                <*> term '+'
                <*> var expr
              ]
  return expr

-- See figure 2 in 
-- http://webhome.cs.uvic.ca/~nigelh/Publications/PracticalEarleyParsing.pdf
badG :: Grammar (Rule Char ())
badG = do
  e <- rule "E" [pure ()]
  a <- rule "A" [term 'a', var e]
  s <- rule "S" [pure (\_ _ _ _ -> ())
                 <*> var a
                 <*> var a
                 <*> var a
                 <*> var a]
  return s

main :: IO ()
main = test grammar "1+0"
