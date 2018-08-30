module ShoppingCartSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Map (Map, (!), fromList)
import qualified Data.Map as Map
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

cartWith3DoveSoapsAnd2AxeDeo = addProductsToCart (addProductsToCart cartWith3DoveSoaps axeDeo) axeDeo

emptyOffers = Map.empty
doveSoapWithOffer = fromList[(doveSoap, buy2Get1FreeOffer)]

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
        totalPrice emptyCart emptyOffers defaultTax `shouldBe` 0.0
    it "should give 139.98 for a dove and axe" $ do
        totalPrice cartWithDoveAndAxe emptyOffers defaultTax `shouldBe` 139.98

  describe "tax price" $ do
    it "should give 35.00 as tax" $ do
        taxPrice cartWith2DoveSoapsAnd2Axe emptyOffers twelvePointFiveTax `shouldBe` 34.995

  describe "offer price" $ do
    it "should give 1 dove soap free if it qualifies for buy 2 get 1 free" $ do
        discountPrice cartWith3DoveSoaps doveSoapWithOffer `shouldBe` 39.99
        totalPrice cartWith3DoveSoaps doveSoapWithOffer twelvePointFiveTax `shouldBe` 89.97749999999999
        taxPrice cartWith3DoveSoaps doveSoapWithOffer twelvePointFiveTax `shouldBe` 9.997499999999999
    it "should give 1 dove soap free when 5 dove soaps are added and dove qualifes for buy 2 get 1 free offer" $ do
        discountPrice cartWith5DoveSoaps doveSoapWithOffer `shouldBe` 39.99
        totalPrice cartWith5DoveSoaps doveSoapWithOffer twelvePointFiveTax `shouldBe` 179.955
        taxPrice cartWith5DoveSoaps doveSoapWithOffer twelvePointFiveTax `shouldBe` 19.995
    it "should not give any discount to axe deo as it does not have any offer" $ do
        discountPrice cartWith3DoveSoapsAnd2AxeDeo doveSoapWithOffer `shouldBe` 39.99
        totalPrice cartWith3DoveSoapsAnd2AxeDeo doveSoapWithOffer twelvePointFiveTax `shouldBe` 314.955
        taxPrice cartWith3DoveSoapsAnd2AxeDeo doveSoapWithOffer twelvePointFiveTax `shouldBe` 34.995



