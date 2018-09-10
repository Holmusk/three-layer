module Test.Core.Password
       ( pwdHashVerify
       ) where

import Hedgehog (MonadGen, assert, forAll)

import Lib.Core.Password (PasswordPlainText (..), mkPasswordHashWithPolicy, verifyPassword)

import Test.Common (Test, prop)

import qualified Crypto.BCrypt as BC
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

pwdHashVerify :: Test
pwdHashVerify = prop "Password verification" $ do
    randomPwd <- forAll genPwd
    let hashPwd = mkPasswordHashWithPolicy BC.fastBcryptHashingPolicy randomPwd
    whenRightM_ (runExceptT hashPwd) $ \pwdHash -> assert $ verifyPassword randomPwd pwdHash

genPwd :: MonadGen m => m PasswordPlainText
genPwd = PwdPlainText <$> Gen.text (Range.constant 8 40) Gen.alphaNum
