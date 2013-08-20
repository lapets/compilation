----------------------------------------------------------------
--
-- | Compilation
--   Monad and combinators for quickly assembling simple
--   compilers.
--
-- @Control\/Compilation\/Module.hs@
--
--   State extension class and combinators for implementations
--   of a state that support module name specification.
--

----------------------------------------------------------------
--

{-# LANGUAGE TypeSynonymInstances #-}

module Control.Compilation.Module
  where

import Control.Compilation

----------------------------------------------------------------
-- | Type synonyms and class memberships.

type ModuleName = String
type StateExtensionModule = ModuleName

----------------------------------------------------------------
-- | State extension class definition, including combinators.

class StateExtension a => HasModule a where
  project :: a -> StateExtensionModule
  inject :: StateExtensionModule -> a -> a

  setModule :: String -> Compilation a ()
  setModule m =
    do s <- get
       set $ inject m s

  getModule :: Compilation a String
  getModule =
    do s <- get
       return $ project s

--eof
