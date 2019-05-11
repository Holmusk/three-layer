{- | We use @hspec@ testing framework and it doesn't support monad transformers.
At this moment there's no testing framework that supports monad transformers. So
we need to pass @AppEnv@ manually to every function.
All functions take `AppEnv` as last argument. Like this one:

@
satisfies :: App a -> (a -> Bool) -> AppEnv -> Expectation
@

Because of that, there're multiple ways to write tests:

@
1. satisfies action isJust env
2. (action `satisfies` isJust) env
3. action `satisfies` isJust $ env
4. env & action `satisfies` isJust
@

We can go even further and introduce fancy operators, so this code can be
written in more concise way. But this is TBD.

@
env & action @? isJust
@
-}

module Test.Assert
       ( failsWith
       , satisfies
       , succeeds
       , equals
       ) where

import Test.Hspec (Expectation, expectationFailure, shouldBe, shouldSatisfy)

import Lib.App (App, AppEnv, AppError (..), AppErrorType, runAppAsIO)


-- | Checks that given action runs successfully.
succeeds :: (Show a) => App a -> AppEnv -> Expectation
succeeds = (`satisfies` const True)

-- | Checks whether return result of the action satisfies given predicate.
satisfies :: (Show a) => App a -> (a -> Bool) -> AppEnv -> Expectation
satisfies app p env = runAppAsIO env app >>= \case
    Left e  -> expectationFailure $ "Expected 'Success' but got: " <> show e
    Right a -> a `shouldSatisfy` p

-- | Checks whether action fails and returns given error.
failsWith :: (Show a) => App a -> AppErrorType -> AppEnv -> Expectation
failsWith app err env = runAppAsIO env app >>= \case
    Left e@AppError{..} ->
        if appErrorType == err
            then appErrorType `shouldBe` err
            else expectationFailure $
                "Expected 'Failure' with: " <> show err <> " but got: " <> show e
    Right a -> expectationFailure $
        "Expected 'Failure' with: " <> show err <> " but got: " <> show a

-- | Checks whether action returns expected value.
equals :: (Show a, Eq a) => App a -> a -> AppEnv -> Expectation
equals app v env = runAppAsIO env app >>= \case
    Right a -> a `shouldBe` v
    Left e  -> expectationFailure $ "Expected 'Success' but got: " <> show e
