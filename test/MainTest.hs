module MainTest where

import Main
import Distribution.Simple.Test
import Test.HUnit

test1 = TestCase (assertEqual "Can read a number" (Number 23) (readExpr "23"))
