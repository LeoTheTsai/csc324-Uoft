{-|
Module: Ex2Types
Description: Types for Exercise 2: Recursive Data Types and Abstract Syntax Trees
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

This module provides the public types required for the Haskell portion of
Exercise 2. You should review the two data types (List and Expr) carefully,
but do not change anything in this file! We will use a fresh copy of this file
for testing purposes.
-}
module Ex2Types
    ( List(..)
    , Expr(..)
    )
where

import Control.Monad (liftM2, liftM3)
import Test.QuickCheck (oneof, listOf1, sized, scale, Arbitrary(..))


-------------------------------------------------------------------------------
-- * Task 1 (List type)
-------------------------------------------------------------------------------

-- | Recursive definition of a /list of integers/ type.
data List = Empty              -- ^ An empty list
          | Cons Integer List  -- ^ An integer "cons'd" with another list
          deriving (Show, Eq)

-- | Here is an example of how to represent the list [1, 2, 3].
listExample :: List
listExample = Cons 1 (Cons 2 (Cons 3 Empty))


-------------------------------------------------------------------------------
-- * Task 2 (Expr type)
-------------------------------------------------------------------------------

data Expr
  = Number Integer      -- ^ A numeric literal
  | Boolean Bool        -- ^ A boolean literal
  | Identifier String   -- ^ An identifier name
  | And [Expr]          -- ^ An (and ...) expression
  | Or [Expr]           -- ^ An (or ...) expression
  | If Expr Expr Expr   -- ^ Ternary if: (if cond then else)
  | Call Expr [Expr]    -- ^ A function call
                        -- (first Expr is the function, [Expr] is the argument list)
  deriving (Show, Eq)

-- | Example: The number 20.
numberExample :: Expr
numberExample = Number 20

-- | Example: (and x #t #f)
andExample :: Expr
andExample = And [Identifier "x", Boolean True, Boolean False]

-- | Example: (if (f 1 2) (g) 40)
ifExample :: Expr
ifExample = If
    (Call (Identifier "f") [Number 1, Number 2])
    (Call (Identifier "g") [])
    (Number 40)


-------------------------------------------------------------------------------
-- * Testing helpers
-- (You can safely ignore ALL of the code below. We don't expect you to
-- understand it at this point.)
-------------------------------------------------------------------------------

-- Specify how to generate a "random" List of a given size.
instance Arbitrary List where
    arbitrary = sized list'
      where
        list' 0 = return Empty
        list' n = oneof [return Empty, liftM2 Cons arbitrary arbitrary]

-- | Specify how to generate a "random" Expr.
instance Arbitrary Expr where
    arbitrary = sized expr'
      where
        expr' 0 = oneof
            [ fmap Number     arbitrary
            , fmap Boolean    arbitrary
            , fmap Identifier arbitrary
            ]
        expr' n = scale (`div` 2) $ oneof
            [ fmap And (listOf1 arbitrary)
            , fmap Or  (listOf1 arbitrary)
            , liftM3 If arbitrary arbitrary arbitrary
            , liftM2 Call arbitrary arbitrary
            ]
