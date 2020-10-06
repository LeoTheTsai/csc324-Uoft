{-|
Module: Ex4
Description: Exercise 4: Strictness Analysis
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}
module Ex4
    ( analyzeStrictness
    )
where
-- Imports the Data.Map library, to be used to represent the environment.
-- See http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html
-- for details.
-- Note that this is a qualified import, meaning you'll need to prefix all
-- identifiers imported from this library with "Map.", e.g. Map.findWithDefault.
import qualified Data.Map.Strict as Map

-- Use the operator ! to do a hash lookup,
-- e.g. `myMap ! myKey` returns the corersponding value
import Data.Map.Strict ((!))

-- Useful for index-based list operations.
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#g:16
import qualified Data.List as List

-- | Imports used for testing purposes only.
import Test.QuickCheck (quickCheck)

import Ex4Types (Prog(..), FuncDef(..), Expr(..))

-------------------------------------------------------------------------------
-- |
-- = Task 1: Strictness analysis
-----------------------------------------------------------------------------
-- |
-- == Your Task: implement `analyzeStrictness` according to the exercise specification.
-- Pay attention to the return type! We've included a link to the relevant documentation above.
--
-- Implementation hints:
--  1.  Same as Exercise 3, the main work can be done by processing the list of
--      function definitions in a call to foldl.
--      (But WARNING: the function passed to foldl takes its arguments in the opposite
--      order as Racket.)
--  2.  Working with list indexes is a bit more tedious in the pure functional world.
--      Use the list function @List.findIndices@, which is similar to @filter@ except
--      it returns indexes rather than elements.
strictnessHelper :: [String] -> Expr -> [Int]
strictnessHelper args (Number body) = []
strictnessHelper args (Identifier body) = List.elemIndices body args
strictnessHelper args (Call funcName body) = foldl (\list body -> list ++ (strictnessHelper args body)) [] body

-- foldl helper: map each funcName with its strict arguments
analyzeStrictnessHelper :: Map.Map String [Int] -> FuncDef -> Map.Map String [Int]
analyzeStrictnessHelper map (FuncDef funcName args body) = Map.insert funcName (strictnessHelper args body) map

-- foldl for each expr, put them in a map
analyzeStrictness :: Prog -> Map.Map String [Int]
analyzeStrictness (Prog funcDefs) = foldl analyzeStrictnessHelper Map.empty funcDefs

-- |
-- @strictIn sMap name expr@ returns whether @name@ is strict
-- in the given expression.
-- It uses the given strictness map to determine strictness for function calls.
--
-- NOTE: this function isn't being tested explicitly, so you may freely change it or
-- ignore it for this exercise.
--
-- Implementation hints:
--   Lookup @List.any@ and @List.all@ to achieve something like "ormap"/"andmap" to a list.
--
--   Remember that you may assume all identifiers are bound.
strictIn :: Map.Map String [Int] -> String -> Expr -> Bool
strictIn sMap x expr = undefined


-------------------------------------------------------------------------------
-- | For testing, we'll use hand-crafted test cases here, parallel to the Racket version.

prop_identity :: Bool
prop_identity = analyzeStrictness (Prog [FuncDef "f" ["x"] (Identifier "x")])
    == Map.fromList [("f", [0])]

prop_oneFunctionPlus :: Bool
prop_oneFunctionPlus =
    analyzeStrictness
            (Prog
                [ FuncDef
                      "f"
                      ["x", "y", "z"]
                      (Call "+" [Identifier "x", Identifier "y"])
                ]
            )
        == Map.fromList [("f", [0, 1])]

prop_twoDependentFunctions :: Bool
prop_twoDependentFunctions =
    analyzeStrictness
            (Prog
                [ FuncDef
                    "f1"
                    ["x", "y", "z"]
                    (Call "+" [Identifier "x", Identifier "y", Identifier "z"])
                , FuncDef
                    "f2"
                    ["a", "b"]
                    (Call "f1" [Identifier "b", Number 5, Number 10])
                ]
            )
        == Map.fromList [("f1", [0, 1, 2]), ("f2", [1])]

-------------------------------------------------------------------------------

-- | This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation much later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
main :: IO ()
main = do
    quickCheck prop_identity
    quickCheck prop_oneFunctionPlus
    quickCheck prop_twoDependentFunctions
