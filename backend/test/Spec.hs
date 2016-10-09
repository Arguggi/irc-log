module Main where

import qualified IrcApi as Api
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Data.Time

main :: IO ()
main = quickCheck maxRangeTime

maxRangeTime :: UTCTime -> Maybe UTCTime -> Maybe UTCTime -> Bool
maxRangeTime now from to = diffUTCTime newTo newFrom <= 3 * 86400
    where  (newFrom, newTo) = Api.dateRange now from to

