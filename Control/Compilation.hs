----------------------------------------------------------------
--
-- Compilation
-- Monads for quickly assembling simple compilers.
--
-- Control/Compilation.hs
--   A generic compilation monad for quickly assembling simple
--   compilers.
--

----------------------------------------------------------------
-- Haskell implementation of a simple compilation monad.

module Control.Compilation
  where

----------------------------------------------------------------
-- Data types and class memberships.

type FreshIndex = Integer
type Indentation = String
type ModuleName = String
type NestingDepth = Integer
type Error = String

data State a = 
  State 
    FreshIndex 
    Indentation 
    (Maybe ModuleName) 
    NestingDepth
    a

empState :: a -> State a
empState s = State 0 "" Nothing 0 s

data Compile a b = 
  Compile (State a -> (State a, b))

-- Standard state monad definition.
instance Monad (Compile a) where
  return x = Compile (\s -> (s, x))
  (>>=) (Compile c1) fc2 = Compile $
    (\state ->
      let (state', r) = c1 state
          Compile c2 = fc2 r
      in c2 state'
    )

----------------------------------------------------------------
-- Generic combinators and functions.

extract :: Compile a () -> a -> a
extract (Compile c) o = let (State _ _ _ _ r, _) = c (empState o) in r

nothing :: Compile a ()
nothing = Compile $ \s -> (s, ())

fresh :: Compile a String
fresh = Compile $ \(State f i m n s) -> (State (f+1) i m n s, show f)

freshWithPrefix :: String -> Compile a String
freshWithPrefix p = Compile $ \(State f i m n s) -> (State (f+1) i m n s, p ++ show f)

setModule :: String -> Compile a ()
setModule m = Compile $ \(State f i _ n s) -> (State f i (Just m) n s, ())

getModule :: Compile a (Maybe String)
getModule = Compile $ \(State f i m n s) -> (State f i m n s, m)

nest :: Compile a ()
nest = Compile $ \(State f i m n s) -> (State f i m (n+1) s, ())

unnest :: Compile a ()
unnest = Compile $ \(State f i m n s) -> (State f i m (n-1) s, ())

depth :: Compile a Integer
depth = Compile $ \(State f i m n s) -> (State f i m n s, n)

--eof
