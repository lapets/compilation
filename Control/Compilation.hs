----------------------------------------------------------------
--
-- | Compilation
--   Monad and combinators for quickly assembling simple
--   compilers.
--
-- @Control\/Compilation.hs@
--
--   A generic compilation monad for quickly assembling simple
--   compilers.
--

----------------------------------------------------------------
--

module Control.Compilation
  where

----------------------------------------------------------------
-- | Data types, class declarations, and class memberships.

class StateExtension a where
  initial :: a

-- | State data structure wrapper.
data State a =
  State a
  
type Compile a b = Compilation a b
data Compilation a b = 
    Compilation (State a -> (State a, b))
  | Error String

-- | Standard state monad definition.
instance StateExtension a => Monad (Compilation a) where
  return x = Compilation (\s -> (s, x))
  (>>=) fc1 fc2 = 
    case fc1 of
      Compilation c1 ->
        Compilation $
          (\state ->
            let (state', r) = c1 state
                Compilation c2 = fc2 r
            in c2 state'
          )
      Error err -> Error err

-- | Default memberships.

instance StateExtension () where
  initial = ()

----------------------------------------------------------------
-- | Generic combinators and functions.

extract :: StateExtension a => Compilation a b -> a
extract (Compilation c) = let (State e, _) = c (State initial) in e

extractFromState :: StateExtension a => a -> Compilation a b -> a
extractFromState s (Compilation c) = let (State e, _) = c (State s) in e

nothing :: Compilation a ()
nothing = Compilation $ \s -> (s, ())

get :: StateExtension a => Compilation a a
get = Compilation $ \(State e) -> (State e, e)

set :: StateExtension a => a -> Compilation a ()
set e = Compilation $ \(State _) -> (State e, ())

error :: String -> Compilation a ()
error err = Error err

--eof
