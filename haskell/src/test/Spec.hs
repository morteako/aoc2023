module Spec where

import Data.Bitraversable (Bitraversable (bitraverse))
import Data.IntMap
import qualified Data.IntMap as Map
import qualified Data.List.Extra as Map
import qualified Day.Day01
import qualified Funcs
import Input (getInput)
import Test.Hspec (describe, hspec, it, runIO, shouldBe)
import Utils

answers :: IntMap (String, String)
answers =
  Map.fromList
    [ 1 =: (,) "802011" "248607374"
    ]

main :: IO ()
main = hspec $ do
  let funcsAndAnswers = Map.intersectionWith (,) Funcs.funcs answers
  flip Map.traverseWithKey funcsAndAnswers $ \k (f, (ansA, ansB)) -> do
    input <- runIO $ getInput k
    describe ("Day" <> show k) $ do
      (actualA, actualB) <- runIO $ f input
      it (show 1) $ do
        actualA `shouldBe` ansA
      it (show 2) $ do
        actualB `shouldBe` ansB
  pure ()