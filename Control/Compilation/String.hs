----------------------------------------------------------------
--
-- Compilation
-- Monad and combinators for quickly assembling simple compilers.
--
-- Control/Compilation/String.hs
--   A generic compilation monad for quickly assembling simple
--   compilers that emit an ASCII string representation of the
--   target language (well-suited for direct syntax translators).
--

----------------------------------------------------------------
-- Haskell combinators for a simple compilation monad.

module Control.Compilation.String
  where
  
import Control.Compilation

----------------------------------------------------------------
-- Combinators and functions for compiling directly into a raw
-- ASCII string.

indent :: Compile String ()
indent = Compile $ \(State f i m n s) -> (State f ("  " ++ i) m n s, ())

unindent :: Compile String ()
unindent = Compile $ \(State f i m n s) -> (State f (drop (min (length i) 2) i) m n s, ())

space :: Compile String ()
space = Compile $ \(State f i m n s) -> (State f i m n (s ++ " "), ())

spaces :: Int -> Compile String ()
spaces k = Compile $ \(State f i m n s) -> (State f i m n (s ++ (take k $ repeat ' ')), ())

newline :: Compile String ()
newline = Compile $ \(State f i m n s) -> (State f i m n (s ++ "\n" ++ i), ())

newlines :: Int -> Compile String ()
newlines k = Compile $ \(State f i m n s) -> (State f i m n (s ++ (take k $ repeat '\n') ++ i), ())

string :: String -> Compile String ()
string s' = Compile $ \(State f i m n s) -> (State f i m n (s ++ s'), ())

raw :: String -> Compile String ()
raw = string

--eof
