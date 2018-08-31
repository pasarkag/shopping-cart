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

emptyCart =  Map.empty
cartWithDoveAndAxe = fromList[(doveSoap, 1), (axeDeo, 1)]
cartWith5DoveSoaps = fromList[(doveSoap, 5)]
cartWith6DoveSoaps = fromList[(doveSoap, 6)]
cartWith2DoveSoapsWithTax = fromList[(doveSoap, 2)]
cartWith2DoveSoapsAnd2Axe = fromList[(doveSoap, 2), (axeDeo, 2)]
cartWith3DoveSoaps = fromList[(doveSoap, 3)]
cartWith2DoveSoaps = fromList[(doveSoap, 2)]
cartWith1DoveSoap = fromList[(doveSoap, 1)]

cartWith3DoveSoapsAnd2AxeDeo = fromList[(doveSoap, 3), (axeDeo, 2)]

emptyOffers = Map.empty
doveSoapWithBuy2Get1Offer = associateOffer emptyOffers doveSoap Buy2Get1Free
doveSoapWithBuy1Get50PercentOffer = associateOffer emptyOffers doveSoap Buy1Get50PercentOnNext

spec :: Spec
spec = do
  describe "add products" $ do
    it "should add single product to cart" $ do
        addProductsToCart emptyCart doveSoap 1 `shouldBe` fromList[(doveSoap, 1)]
    it "should add same product multiple times to the cart" $ do
        addProductsToCart (addProductsToCart emptyCart doveSoap 1) doveSoap 2 `shouldBe` fromList[(doveSoap, 3)]
    it "should add multiple products" $ do
        addProductsToCart (addProductsToCart emptyCart doveSoap 1) axeDeo 1 `shouldBe` fromList[(doveSoap, 1), (axeDeo, 1)]
    it "should be able to fetch quantity of a product" $ do
        findProductQuantity cartWith6DoveSoaps doveSoap `shouldBe` 6

  describe "total price" $ do
    it "should give 0.0 for an empty shopping cart" $ do
        totalPrice emptyCart emptyOffers defaultTax `shouldBe` 0.0
    it "should give 139.98 for a dove and axe" $ do
        totalPrice cartWithDoveAndAxe emptyOffers defaultTax `shouldBe` 139.98

  describe "tax price" $ do
    it "should give 35.00 as tax" $ do
        taxPrice cartWith2DoveSoapsAnd2Axe emptyOffers twelvePointFiveTax `shouldBe` 35.00

  describe "offer price for buy 2 get 1 free" $ do
    it "should give 1 dove soap free" $ do
        discountPrice cartWith3DoveSoaps doveSoapWithBuy2Get1Offer `shouldBe` 39.99
        totalPrice cartWith3DoveSoaps doveSoapWithBuy2Get1Offer twelvePointFiveTax `shouldBe` 89.98
        taxPrice cartWith3DoveSoaps doveSoapWithBuy2Get1Offer twelvePointFiveTax `shouldBe` 10.00
    it "should give 1 dove soap free when 5 dove soaps are added" $ do
        discountPrice cartWith5DoveSoaps doveSoapWithBuy2Get1Offer `shouldBe` 39.99
        totalPrice cartWith5DoveSoaps doveSoapWithBuy2Get1Offer twelvePointFiveTax `shouldBe` 179.96
        taxPrice cartWith5DoveSoaps doveSoapWithBuy2Get1Offer twelvePointFiveTax `shouldBe` 20.00
    it "should not give any discount to axe deo as it does not have any offer" $ do
        discountPrice cartWith3DoveSoapsAnd2AxeDeo doveSoapWithBuy2Get1Offer `shouldBe` 39.99
        totalPrice cartWith3DoveSoapsAnd2AxeDeo doveSoapWithBuy2Get1Offer twelvePointFiveTax `shouldBe` 314.96
        taxPrice cartWith3DoveSoapsAnd2AxeDeo doveSoapWithBuy2Get1Offer twelvePointFiveTax `shouldBe` 35.00

  describe "offer price for buy 1 get 50% on next" $ do
    it "should give 50% discount on second dove soap when 2 are added" $ do
        discountPrice cartWith2DoveSoaps doveSoapWithBuy1Get50PercentOffer `shouldBe` 20.00
        totalPrice cartWith2DoveSoaps doveSoapWithBuy1Get50PercentOffer twelvePointFiveTax `shouldBe` 67.48
        taxPrice cartWith2DoveSoaps doveSoapWithBuy1Get50PercentOffer twelvePointFiveTax `shouldBe` 7.50

  describe "buy 2 get 1 free offer" $ do
    it "should NOT give any discount for 1 item" $ do
        applyOffer Buy2Get1Free cartWith1DoveSoap doveSoap `shouldBe` 0.0
    it "should NOT give any discount for 2 items" $ do
        applyOffer Buy2Get1Free cartWith2DoveSoaps doveSoap `shouldBe` 0.0
    it "should give discount on 1 item when 3 are added" $ do
        applyOffer Buy2Get1Free cartWith3DoveSoaps doveSoap `shouldBe` 39.99

  describe "buy 1 get 50% on next offer" $ do
    it "shouuld NOT give any discount for 1 item" $ do
      applyOffer Buy1Get50PercentOnNext cartWith1DoveSoap doveSoap `shouldBe` 0.0
    it "should give 50% discount for 1 item when 2 are added" $ do
      applyOffer Buy1Get50PercentOnNext cartWith2DoveSoaps doveSoap `shouldBe` 20.00


