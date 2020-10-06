{-|
Module: Ex2
Description: Exercise 2: Recursive Data Types and Abstract Syntax Trees
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

*Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html*
-}

-- The module definition line, including exports. Don't change this!
module Ex2
    (
    -- Task 1
      numEvensList
    , mapList
    ,
    -- Task 2
      tailCalls
    )
where
import Test.QuickCheck (quickCheck)
import Ex2Types (List(..), Expr(..))

-------------------------------------------------------------------------------
-- |
-- * Task 1: A recursive type definition
-------------------------------------------------------------------------------

-- | numEvensList returns the number of even items in a List.
-- Here you'll need to use pattern-matching on the different forms of a List
-- (@Empty@ or @Cons@), because we don't have an explicit "is-empty?" function
-- for this datatype.
numEvensList :: List -> Integer
numEvensList Empty =
    -- In this case, the list is empty.
    0
numEvensList (Cons first rest) =
    -- In this case, @first@ is an integer representing the first item in the list,
    -- and @rest@ is a List representing the other items in the list.
    if 0 == (rem first 2)
    then 
      1 + numEvensList rest
    else
      numEvensList rest


-- | mapList behaves the same as @map@ for built-in lists in Haskell.
mapList :: (Integer -> Integer) -> List -> List
mapList _ Empty = undefined
mapList f (Cons first rest) = (Cons (f first) (mapList f rest))


-------------------------------------------------------------------------------
-- | What does this property mean?
prop_numEvensFirstOdd :: List -> Bool
prop_numEvensFirstOdd nums = numEvensList nums == numEvensList (Cons 1 nums)

-- | What does this property mean? (Hint: `id` is the identity function.)
prop_mapIdentity :: List -> Bool
prop_mapIdentity nums =
  -- Note: the reason we can use (==) here to compare Lists
  -- is because of the "deriving Eq" in the type definition.
  -- More on this later in the course.
    mapList id nums == nums

-- | What does this property mean?
prop_mapAdd2 :: List -> Bool
prop_mapAdd2 nums =
    let newNums = mapList (\x -> x + 2) nums
    in numEvensList nums == numEvensList newNums


-------------------------------------------------------------------------------
-- |
-- * Task 2: Tail call positions
-------------------------------------------------------------------------------

-- | Your Task: implement @tailCalls@ according to the exercise specification.
-- We've started the structural pattern-matching for you; please modify as needed.
tailCalls :: Expr -> [Expr]
tailCalls (Number n) = []
tailCalls (Boolean b) = []
tailCalls (Identifier ident) = []
tailCalls (And exprs) = 
        if exprs == []
        then
            []
        else
            tailCalls (last expr)
tailCalls (Or exprs) = 
        if exprs == []
        then
            []
        else
            tailCalls (last expr)
tailCalls (If condExpr thenExpr elseExpr) = 
        (tailCalls thenExpr) ++ (tailCalls elseExpr)
tailCalls (Call funcExpr args) = [Call funcExpr args]


-------------------------------------------------------------------------------
-- | This property checks that the only values in the output of `tailCalls`
-- are function call expressions. (You can also try porting over some tests
-- from the Racket starter code, and create your own new tests!)
prop_returnsAllCalls :: Expr -> Bool
prop_returnsAllCalls expr = all isCall (tailCalls expr)
  where
    isCall :: Expr -> Bool
    isCall (Call _ _) = True
    isCall _          = False

-------------------------------------------------------------------------------

-- | This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation much later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
main :: IO ()
main = do
    quickCheck prop_numEvensFirstOdd
    quickCheck prop_mapIdentity
    quickCheck prop_mapAdd2
    quickCheck prop_returnsAllCalls
