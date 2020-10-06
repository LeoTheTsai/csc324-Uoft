{-|
Module: Ex3
Description: Exercise 3: More with Higher-Order Functions; Building Environments
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}
-- The module definition line, including exports. Don't change this!
module Ex3
    ( buildEnv
    )
where

-- Imports the Data.Map library, to be used to represent the environment.
-- See <http://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html> for details.
-- Note that this is a qualified import, meaning you'll need to prefix all
-- identifiers imported from this library with "Map.", e.g. Map.findWithDefault.
import qualified Data.Map.Strict as Map

import Ex3Types (Prog(..), Expr(..), Binding(..))

-- Imports used for testing purposes only.
import Test.QuickCheck (quickCheck)

-------------------------------------------------------------------------------
-- |
-- * Task 1: Building an environment
-------------------------------------------------------------------------------

-- | Build an environment from a list of bindings.
-- Hint: this can be done with a single call to `foldl`,
-- as long as you define the appropriate "update" function.
buildEnv :: Prog -> Map.Map String Integer
buildEnv (Prog bindings) = foldl helper Map.empty bindings

helper :: Map.Map String Integer -> Binding -> Map.Map String Integer
helper map (Binding key (Identifier value)) =
    Map.insert key (map Map.! value) map

helper map (Binding key (Number value)) = 
    Map.insert key value map




-- | This is a sample test that checks a bunch of independent name bindings
-- (each variable is bound to a different numeric literal).
prop_onlyIntegers :: Bool
prop_onlyIntegers =
    let
        n        = 100
        names    = [ "x" ++ show i | i <- [1 .. n] ]
        numbers  = [n, n - 1 .. 1]
        bindings = zipWith (\name m -> Binding name (Number m)) names numbers
        expected = Map.fromList (zip names numbers)
    in buildEnv (Prog bindings) == expected


-------------------------------------------------------------------------------

-- | This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation much later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
main :: IO ()
main = do
    quickCheck prop_onlyIntegers
