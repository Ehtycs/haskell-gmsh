module MarshallerSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Coerce (coerce)

import Test.QuickCheck
import Test.QuickCheck.Monadic

import GmshAPI
import Foreign.Marshal.Array
import Foreign.Marshal
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

tol = 1e-6

withPeekArrayArrayTest withF peekF lstlst' = monadicIO $ do
      let lstlst = coerce lstlst'
      out <- run $ withF lstlst $ \nn n lst -> do
         with nn $ \nn' -> do
            with n $ \n' -> do
               with lst $ \lst' -> do
                  peekF nn' n' lst'
      assert $ lstlst == out

spec :: Spec
spec = do
   describe "withArrayArrayDoubleLen/peekArrayArrayDouble" $ do
      it "withArrayArrayDoubleLen -> peekArrayArrayDouble is id" $ property $ test
      where
         test :: NonEmptyList (NonEmptyList Double) -> Property
         test = withPeekArrayArrayTest withArrayArrayDoubleLen peekArrayArrayDouble

   describe "withArrayArrayInt/peekArrayArrayInt" $ do
      it "withArrayArrayIntLen -> peekArrayArrayInt is id" $ property $ test
      where
         test :: NonEmptyList (NonEmptyList Int) -> Property
         test = withPeekArrayArrayTest withArrayArrayIntLen peekArrayArrayInt
