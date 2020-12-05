import qualified Control.Monad.State.Strict as State
import           Data.Text (pack)
import           Data.Time (UTCTime, addUTCTime, defaultTimeLocale, getCurrentTime, nominalDay,
                            parseTimeOrError)
import           Lib.Core.Account (Account, getAccountNo, mkAccount)
import           Lib.Core.DomainError (DomainError)
import           Lib.Repository.AccountRepo (AccountRepo (getAccountByUserId, insertAccount))
import qualified Relude.Unsafe as Unsafe
import           Test.Hspec (describe, hspec, it)
import           Test.Hspec.Expectations (shouldBe)
import           Validation (failures, successes)

timeFormat :: String
timeFormat = "%H:%M:%S"
understandTime :: String -> UTCTime
understandTime = parseTimeOrError True defaultTimeLocale timeFormat

anHourFromNow :: IO UTCTime
anHourFromNow = do addUTCTime 3600 <$> getCurrentTime

anHourBackFromNow :: IO UTCTime
anHourBackFromNow = do addUTCTime (-3600) <$> getCurrentTime

accounts :: IO [Account]
accounts = do
  c <- getCurrentTime
  let a1 = mkAccount c "a-01234567" "a-name-1" (addUTCTime (-3600) c) Nothing (pack "u-001")
  let a2 = mkAccount c "a-12345678" "a-name-2" (addUTCTime (-3600) c) Nothing (pack "u-002")
  return $ successes [a1, a2]

oneAccount :: Text -> IO [Account]
oneAccount ano = do
  c <- getCurrentTime
  let a1 = mkAccount c ano "a-name-3" (addUTCTime (-3600) c) Nothing (pack "u-003")
  return $ successes [a1]

invalidAccounts :: IO [NonEmpty DomainError]
invalidAccounts = do
  c <- getCurrentTime
  let a1 = mkAccount c "a-012345" "a-name-1" (addUTCTime nominalDay c) Nothing (pack "u-001")
  let a2 = mkAccount c "a-123456" "a-name-2" (addUTCTime nominalDay c) Nothing (pack "u-002")
  return $ failures [a1, a2]

main :: IO ()
main = hspec $ do
  describe "getAccountByUserId" $ do
    it "returns the account corresponding to the user id passed" $ do
      accs <- accounts
      let res = State.evalState (getAccountByUserId "u-001") accs
      res `shouldBe` Unsafe.head accs

  describe "account validation" $ do
    it "fails for all accounts" $ do
      accs <- invalidAccounts
      (length accs `shouldBe` 2) >> print accs

  describe "account add" $ do
    it "should add the account" $ do
      accs <- accounts
      acc <- oneAccount "a-76543210"
      let res = State.execState (insertAccount (Unsafe.head acc)) accs
      (length res `shouldBe` 3) >> print res

    it "should fail adding an account with duplicate account no" $ do
      accs <- accounts
      acc <- oneAccount (getAccountNo $ Unsafe.head accs)
      let res = State.execState (insertAccount (Unsafe.head acc)) accs
      (length res `shouldBe` 2) >> print res

