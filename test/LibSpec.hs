module LibSpec
  ( main
  , spec
  ) where

import Control.Monad
import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunc" $ do
    it "is equivalent to succ for Int" $ property $ \n ->
      someFunc n == succ n
    forM_
      [ (0, 1)
      , (1, 2)
      ] $ \(m, n) ->
        it ("should return " ++ show n ++ " when given " ++ show m) $ someFunc m `shouldBe` n
