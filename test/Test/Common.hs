module Test.Common
       ( Test
       , joinSpecs
       , prop
       ) where

import Hedgehog (PropertyT, property)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, testSpecs)

-- | Convenient type alias to not have extra import
type Test = IO TestTree

-- | Helper function to create property tests.
prop :: TestName -> PropertyT IO () -> Test
prop name = pure . testProperty name . property

-- | Joins list of specs into single test group with given name.
joinSpecs :: TestName -> [Spec] -> Test
joinSpecs name specs = testGroup name <$> foldMapA testSpecs specs
