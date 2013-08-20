----------------------------------------------------------------
--
-- | Compilation
--   Monad and combinators for quickly assembling simple
--   compilers.
--
-- @Control\/Compilation\/Sequences.hs@
--
--   A generic compilation monad for quickly assembling simple
--   compilers for target languages that are primarily
--   sequences of instructions (possibly with nesting, e.g., 
--   loop constructs or procedures).
--

----------------------------------------------------------------
--

{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Control.Compilation.Sequences
  where

import Control.Compilation

----------------------------------------------------------------
-- | Type synonyms and class memberships.

type StateExtensionSequences a = [[a]]

instance StateExtension (StateExtensionSequences a) where
  initial = []

----------------------------------------------------------------
-- | State extension class definition, and combinators for
--   compiling into a sequence (possibly with nested blocks)
--   of instructions.

class StateExtension a => HasSequences a b where
  project :: a -> StateExtensionSequences b
  inject :: StateExtensionSequences b -> a -> a

  nest :: [b] -> Compilation a ()
  nest xs =
    do s :: a <- get
       xss :: StateExtensionSequences b <- return $ project s
       set $ inject (xs : xss) s

  unnest :: Compilation a [b]
  unnest =
    do s :: a <- get
       xs :: StateExtensionSequences b <- return $ project s
       set $ inject (tail $ xs) s
       return $ head $ project s

  depth :: Compilation a Integer
  depth =
    do s <- get
       xss :: StateExtensionSequences b <- return $ project s
       return $ toInteger $ length xss

--eof
