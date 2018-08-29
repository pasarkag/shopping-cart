module ShoppingCartSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import ShoppingCart

main :: IO ()
main = hspec spec

defaultTax = 0.0
emptyCart = MyCart [] defaultTax
doveSoap = CartProduct "Dove" 39.99
axeDeo = CartProduct "Axe Deo" 99.99

cartWithDoveAndAxe = addProductsToCart (addProductsToCart emptyCart doveSoap) axeDeo
fiveDoveSoaps = replicate 5 doveSoap
cartWith6DoveSoaps = (addProductsToCart (MyCart fiveDoveSoaps defaultTax) doveSoap)

twelvePointFiveTax = 0.125
twoDoveSoaps = (replicate 2 doveSoap)
cartWith2DoveSoapsWithTax = MyCart twoDoveSoaps twelvePointFiveTax
cartWith2DoveSoapsAnd2Axe = addProductsToCart (addProductsToCart cartWith2DoveSoapsWithTax axeDeo) axeDeo

threeDoveSoaps = (replicate 3 doveSoap)
cartWith3DoveSoaps = MyCart threeDoveSoaps twelvePointFiveTax

spec :: Spec
spec = do
  describe "add products" $ do
    it "should add single product to cart" $ do
        addProductsToCart emptyCart doveSoap `shouldBe` MyCart [doveSoap] defaultTax
    it "should add multiple products" $ do
        addProductsToCart (addProductsToCart emptyCart doveSoap) axeDeo `shouldBe` MyCart [doveSoap, axeDeo] defaultTax
    it "should be able to fetch quantity of a product" $ do
        findProductQuantity cartWith6DoveSoaps doveSoap `shouldBe` 6

  describe "total price" $ do
    it "should give 0.0 for an empty shopping cart" $ do
        totalPrice emptyCart `shouldBe` 0.0
    it "should give 139.98 for a dove and axe" $ do
        totalPrice cartWithDoveAndAxe `shouldBe` 139.98

  describe "tax price" $ do
    it "should give 35.00 as tax" $ do
        taxPrice cartWith2DoveSoapsAnd2Axe `shouldBe` 34.995
