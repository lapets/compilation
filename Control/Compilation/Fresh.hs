----------------------------------------------------------------
--
-- | Compilation
--   Monad and combinators for quickly assembling simple
--   compilers.
--
-- @Control\/Compilation\/Fresh.hs@
--
--   State extension class and combinators for implementations
--   of a state that support generation of fresh (i.e., unique)
--   values (integers and strings).
--

----------------------------------------------------------------
--

{-# LANGUAGE TypeSynonymInstances #-}

module Control.Compilation.Fresh
  where

import Control.Compilation

----------------------------------------------------------------
-- | Type synonyms and class memberships.

type FreshIndex = Integer
type StateExtensionFresh = FreshIndex

instance StateExtension StateExtensionFresh where
  initial = 0

----------------------------------------------------------------
-- | State extension class definition, including combinators
--   and convenient synonyms.

class StateExtension a => HasFresh a where
  project :: a -> StateExtensionFresh
  inject :: StateExtensionFresh -> a -> a

  freshInteger :: Compilation a Integer
  freshInteger =
    do s <- get
       i <- return $ project s
       set $ inject (i+1) s
       return $ i

  freshString :: Compilation a String
  freshString =
    do i <- freshInteger
       return $ show i

  freshStringWithPrefix :: String -> Compilation a String
  freshStringWithPrefix prefix =
    do s <- freshString
       return $ prefix ++ s

  freshWithPrefix :: String -> Compilation a String
  freshWithPrefix = freshStringWithPrefix

  fresh :: Compilation a String
  fresh = freshString

  fresh_ :: String -> Compilation a String
  fresh_ = freshStringWithPrefix

  freshes :: Integer -> Compilation a [String]
  freshes i =
    do ns <- mapM (\_ -> fresh) [0..i-1]
       return $ [show n | n <- ns]

  freshes_ :: String -> Integer -> Compilation a [String]
  freshes_ prefix i =
    do ns <- mapM (\_ -> fresh) [0..i-1]
       return $ [prefix ++ show n | n <- ns]
       
--eof
