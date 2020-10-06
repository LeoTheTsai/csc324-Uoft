{-|
Module: Ex3Types
Description: Types for Exercise 3: More with Higher-Order Functions; Building Environments
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

This module provides the public types required for the Haskell portion of
Exercise 3. You should review the data types (Binding and Expr) carefully,
but do not change anything in this file! We will use a fresh copy of this file
for testing purposes.
-}
module Ex3Types
    ( Prog(..)
    , Expr(..)
    , Binding(..)
    )
where


-------------------------------------------------------------------------------
-- * Task 1 (Building an environment)
-------------------------------------------------------------------------------
data Prog = Prog [Binding]     -- ^ A program is a list of bindings.
          deriving (Show, Eq)

data Expr
  = Number Integer      -- ^ A numeric literal.
  | Identifier String   -- ^ An identifier.
  deriving (Show, Eq)

data Binding
  = Binding String Expr -- ^ A binding of an identifier name to an expression.
  deriving (Show, Eq)
