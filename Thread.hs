{-# OPTIONS -XExistentialQuantification #-}

module Thread (Thread, expect, feed, force) where

import Control.Applicative
import Data.Dynamic

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
