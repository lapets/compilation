----------------------------------------------------------------
--
-- | Compilation
--   Monad and combinators for quickly assembling simple
--   compilers.
--
-- @Control\/Compilation\/Environment.hs@
--
--   State extension class and combinators for implementations
--   of a state that support an environment (i.e., lookup table
--   or dictionary) data structure or structures.
--

----------------------------------------------------------------
--

{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Control.Compilation.Environment
  where

import Control.Compilation

----------------------------------------------------------------
-- | Type synonyms and class memberships.

type StateExtensionEnv a = [(String, a)]

instance StateExtension (StateExtensionEnv a) where
  initial = []

----------------------------------------------------------------
-- | State extension class definition.

class StateExtension a => HasEnvironment a b where
  project :: a -> StateExtensionEnv b
  inject :: StateExtensionEnv b -> a -> a

  addEnv :: String -> b -> Compilation a ()
  addEnv v x =
    do s :: a <- get
       env :: StateExtensionEnv b <- return $ project s
       set $ inject ((v,x):env) s

  popEnv :: Compilation a ()
  popEnv =
    do s :: a <- get
       env :: StateExtensionEnv b <- return $ project s
       set $ inject (tail env) s

  dropEnv :: Int -> Compilation a ()
  dropEnv n =
    do s :: a <- get
       env :: StateExtensionEnv b <- return $ project s
       set $ inject (drop n env) s

  lookupEnv :: String -> Compilation a (Maybe b)
  lookupEnv v =
    do s :: a <- get
       env :: StateExtensionEnv b <- return $ project s
       return $ lookup v env

  setEnv :: StateExtensionEnv b -> Compilation a ()
  setEnv env =
    do s :: a <- get
       set $ inject env s

  getEnv :: Compilation a (StateExtensionEnv b)
  getEnv =
    do s :: a <- get
       env :: StateExtensionEnv b <- return $ project s
       return $ env

--eof
