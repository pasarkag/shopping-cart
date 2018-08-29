module ShoppingCartSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import ShoppingCart

main :: IO ()
main = hspec spec

defaultTax = 0.0
twelvePointFiveTax = 0.125

doveSoap = CartProduct "Dove" 39.99
axeDeo = CartProduct "Axe Deo" 99.99

emptyCart =  []
cartWithDoveAndAxe = addProductsToCart (addProductsToCart emptyCart doveSoap) axeDeo
cartWith5DoveSoaps = replicate 5 doveSoap
cartWith6DoveSoaps = addProductsToCart cartWith5DoveSoaps doveSoap
cartWith2DoveSoapsWithTax = (replicate 2 doveSoap)
cartWith2DoveSoapsAnd2Axe = addProductsToCart (addProductsToCart cartWith2DoveSoapsWithTax axeDeo) axeDeo
cartWith3DoveSoaps = (replicate 3 doveSoap)

spec :: Spec
spec = do
  describe "add products" $ do
    it "should add single product to cart" $ do
        addProductsToCart emptyCart doveSoap `shouldBe` [doveSoap]
    it "should add multiple products" $ do
        addProductsToCart (addProductsToCart emptyCart doveSoap) axeDeo `shouldBe` [doveSoap, axeDeo]
    it "should be able to fetch quantity of a product" $ do
        findProductQuantity cartWith6DoveSoaps doveSoap `shouldBe` 6

  describe "total price" $ do
    it "should give 0.0 for an empty shopping cart" $ do
        totalPrice emptyCart defaultTax `shouldBe` 0.0
    it "should give 139.98 for a dove and axe" $ do
        totalPrice cartWithDoveAndAxe defaultTax `shouldBe` 139.98

  describe "tax price" $ do
    it "should give 35.00 as tax" $ do
        taxPrice cartWith2DoveSoapsAnd2Axe twelvePointFiveTax `shouldBe` 34.995
