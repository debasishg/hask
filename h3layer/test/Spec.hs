import qualified Control.Monad.State.Strict as State
import qualified Relude.Unsafe as Unsafe
import           Data.Text (pack)
import           Data.Time (UTCTime, defaultTimeLocale, getCurrentTime, parseTimeOrError)
import           Lib.Core.Account (Account, mkAccount)
import           Lib.Repository.AccountRepo (AccountRepo (getAccountByUserId))
import           Test.Hspec (describe, hspec, it)
import           Test.Hspec.Expectations (shouldBe)
import           Validation (successes)

timeFormat :: String
timeFormat = "%H:%M:%S"
understandTime :: String -> UTCTime
understandTime = parseTimeOrError True defaultTimeLocale timeFormat

accounts :: IO [Account]
accounts = do
  c <- getCurrentTime
  let a1 = mkAccount c "a-01234567" "a-name-1" (understandTime "10:30:20") Nothing (pack "u-001")
  let a2 = mkAccount c "a-12345678" "a-name-2" (understandTime "10:30:20") Nothing (pack "u-002")
  return $ successes [a1, a2]

main :: IO ()
main = hspec $ do
  describe "getAccountByUserId" $ do
    it "works" $ do
      accs <- accounts
      let res = State.evalState (getAccountByUserId "u-001") accs
      res `shouldBe` Unsafe.head accs
