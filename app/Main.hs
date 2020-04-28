module Main where

import Control.Monad
import Control.Applicative
import Control.Monad.Logic

data Gender = Male | Female deriving (Eq, Show)

data Answer = Answer {
    parent1 :: Gender
  , parent2 :: Gender
  , child :: Gender
  , child_said :: Gender
  } deriving (Show)

child_statement Male Female = False
child_statement _ _ = True

parent1_statement Male Female = False
parent1_statement _ _ = True

parent2_statement Male Female False = True
parent2_statement Male _ _ = False
parent2_statement Female Female False = False
parent2_statement Female Male True = False
parent2_statement Female _ _ = True
-- logict

solve :: [Answer]
solve = do
  parent1 <- [Male, Female]
  parent2 <- [Male, Female]
  child <- [Male, Female]
  child_said <- [Male, Female]
  guard $ parent1 /= parent2
  guard $ child_statement child child_said
  guard $ parent1_statement parent1 child_said
  guard $ parent2_statement
    parent2 child (child == child_said)
  return Answer {
      parent1 = parent1
    , parent2 = parent2
    , child = child
    , child_said = child_said
    }

numbers :: Logic Integer
numbers = msum (map pure [2,3..])

iota :: Integer -> Logic Integer
iota i = msum (map pure [2,3..i-1])

notPrime :: Logic Integer
notPrime = do
  n <- numbers
  ifte (once $ do
    d <- iota n
    guard $ n `mod` d == 0
    )
    (const $ return n)
    mzero

prime :: Logic Integer
prime = do
  n <- numbers
  ifte (once $ do
    d <- iota n
    guard $ n `mod` d == 0
    )
    (const empty)
    (return n)

data Person =
  Tim | Tom | John | Mary | Isabelle
  | Judy | Frank | Miranda
  deriving (Show, Eq, Bounded, Enum)

allPeople :: Logic Person
allPeople = msum (map pure [minBound..maxBound])

male :: Logic Person
male = msum (map pure [Tim, Tom, John, Frank])

female :: Logic Person
female = do
  p <- allPeople
  ifte (do
    m <- male
    guard $ m == p
    )
    (const empty)
    (pure p)

mother :: Person -> Logic Person
mother Frank = pure Mary
mother Judy = pure Mary
mother Isabelle = pure Mary
mother Tom = pure Judy
mother _ = empty

married' :: Person -> Logic Person
married' Mary = pure Tim
married' Judy = pure John
married' Frank = pure Miranda
married' _ = empty

married :: Person -> Logic Person
married x = married' x `interleave`
  (allPeople
  >>- (\p -> married' p
  >>- (\s -> guard (s == x)
  >>- (\_ -> pure p))))

father :: Person -> Logic Person
father x = do
  m <- mother x
  married m

isMale :: Person -> Logic ()
isMale x = once $ do
  m <- male
  guard $ m == x

children :: Person -> Logic Person
children x =
  ifte (isMale x)
    (const $ do
        p <- allPeople
        ifte (once $ do
          f <- father p
          guard $ f == x)
          (const $ pure p)
          empty
    )
    (do
      p <- allPeople
      ifte (once $ do
        f <- mother p
        guard $ f == x)
        (const $ pure p)
        empty
    )

siblings :: Person -> Logic Person
siblings x = do
  m <- mother x
  c <- children m
  guard $ c /= x
  return c

brothers :: Person -> Logic Person
brothers x = do
  s <- siblings x
  ifte (isMale s)
    (const $ pure s)
    empty

sisters :: Person -> Logic Person
sisters x = do
  s <- siblings x
  ifte (isMale s)
    (const empty)
    (pure s)

main :: IO ()
main = undefined
