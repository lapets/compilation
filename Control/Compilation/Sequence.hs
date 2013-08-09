----------------------------------------------------------------
--
-- Compilation
-- Monad and combinators for quickly assembling simple compilers.
--
-- Control/Compilation/Sequence.hs
--   A generic compilation monad for quickly assembling simple
--   compilers for target languages that are primarily
--   sequences of instructions (possibly with nesting, e.g., 
--   loop constructs or procedures).
--

----------------------------------------------------------------
-- Haskell combinators for a simple compilation monad.

module Control.Compilation.Sequence
  where

----------------------------------------------------------------
-- Combinator definitions.

ccsDummy = ()

--eof
