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

test :: Show a => Parser t a -> [t] -> IO ()
test (Parser _) [] = putStrLn "FAILED, EOF"
test (Parser n) (x:xs) = do
  (res, p) <- n x
  case res of
    Just v -> do putStr "MATCH:"
                 putStrLn (show v)
    Nothing -> return ()
  test p xs

tp :: (Show a, Typeable a, Typeable t) => G.Grammar (G.Rule t a) -> [t] -> IO ()
tp p i =
  let pa = (parse p) in
  test pa i
