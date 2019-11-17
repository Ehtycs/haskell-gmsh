module MarshallerSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import GmshAPI
import Foreign.Marshal.Array
import Foreign.Marshal

tol = 1e-6

isClose :: (Ord a, Fractional a) => a -> a -> Bool
isClose x y = (abs $ x-y) < realToFrac tol

lstIsClose :: (Ord a, Fractional a) => [a] -> [a] -> Bool
lstIsClose xs ys = all id $ zipWith isClose xs ys

lstLstIsClose :: [[Double]] -> [[Double]] -> Bool
lstLstIsClose xss yss = all id (zipWith lstIsClose xss yss)


spec :: Spec
spec = do
   describe "withArrayArrayLen/peekArrayArrayLen" $ do
      prop "withArrayArrayLen -> peekArrayArrayLen is id" $ \lstlst -> do
         out <- withArrayArrayDoubleLen lstlst $ \nn n lst -> do
            with nn $ \nn' -> do
               with n $ \n' -> do
                  with lst $ \lst' -> do
                     peekArrayArrayDouble nn' n' lst'
         --True `shouldBe` True
         -- putStrLn "---"
         -- print lstlst
         -- print out
         (lstLstIsClose lstlst out) `shouldBe` True
