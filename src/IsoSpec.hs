
module IsoSpec where

import Test.Hspec
import Iso

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "" $ do
      it "" $ do
        "" `shouldBe` ""