module ShoppingCart (
  addProductsToCart,
  totalPrice,
  taxPrice,
  findProductQuantity,
  Product(CartProduct),
  Cart,
  Offer(Buy2Get1Free, Buy1Get50PercentOnNext),
  discountPrice,
  associateOffer,
  applyOffer
) where

import Data.Map (Map, (!), foldrWithKey, insertWith, insert)
import Data.Decimal (Decimal, roundTo)

data Product = CartProduct{
  name :: String,
  price :: Price
} deriving (Show, Eq, Ord)

type Price = Decimal

type ProductCount = Int

type Cart = Map Product ProductCount

data Offer = Buy2Get1Free | Buy1Get50PercentOnNext
  deriving (Eq)

type OfferAssociation = Map Product Offer

defaultPrice = 0.0

addProductsToCart :: Cart -> Product -> ProductCount -> Cart
addProductsToCart cart product count =  insertWith (+) product count cart

associateOffer :: OfferAssociation -> Product -> Offer -> OfferAssociation
associateOffer offers product offer = insert product offer offers

totalPrice :: Cart -> OfferAssociation -> Price -> Price
totalPrice cart offers taxRate = roundToTwoDecimals $ total + (total * taxRate)
  where total = discountedCartSum cart offers

discountedCartSum :: Cart -> OfferAssociation -> Price
discountedCartSum cart offers = roundToTwoDecimals $ foldrWithKey computePrice defaultPrice cart - (discountPrice cart offers)
  where computePrice product count acc = acc + ((price product) * fromIntegral count)

taxPrice :: Cart -> OfferAssociation -> Price -> Price
taxPrice cart offers taxRate = roundToTwoDecimals $ (discountedCartSum cart offers) * (taxRate)

discountPrice :: Cart -> OfferAssociation -> Price
discountPrice cart offers = roundToTwoDecimals $ foldrWithKey computeDiscount defaultPrice offers
  where computeDiscount product offer acc = acc + applyOffer offer cart product

roundToTwoDecimals input = roundTo 2 input

findProductQuantity :: Cart -> Product -> ProductCount
findProductQuantity cart product = cart ! product

applyOffer :: Offer -> Cart -> Product -> Price
applyOffer productOffer cart product
          | productOffer == Buy2Get1Free = roundToTwoDecimals (buy2Get1FreeOffer cart product)
          | productOffer == Buy1Get50PercentOnNext = roundToTwoDecimals (buy1Get50PercentOnNext cart product)
          | otherwise = defaultPrice

buy2Get1FreeOffer cart product = ((price product) * discountOn)
  where discountOn = fromIntegral $ floor $ fromIntegral (findProductQuantity cart product) / 3

buy1Get50PercentOnNext cart product = discountOn * ((price product) / 2)
  where discountOn = fromIntegral $ floor $ fromIntegral (findProductQuantity cart product) / 2

