module ShoppingCart (
  addProductsToCart,
  totalPrice,
  taxPrice,
  findProductQuantity,
  Product(CartProduct),
  Cart,
  buy2Get1FreeOffer,
  discountPrice
) where

import Data.Map (Map, (!), foldrWithKey, insertWith)
import qualified Data.Map as Map

data Product = CartProduct{
  name :: String,
  price :: Double
} deriving (Show, Eq, Ord)

type Cart = Map Product Int

type Offer = Cart -> Product -> Double

type OfferAssociation = Map Product Offer

addProductsToCart :: Cart -> Product -> Int -> Cart
addProductsToCart cart product count =  insertWith (+) product count cart

totalPrice :: Cart -> OfferAssociation -> Double -> Double
totalPrice cart offers taxRate = total + (total * taxRate) where
    total = (cartSum cart) - (discountPrice cart offers)

cartSum :: Cart -> Double
cartSum cart = foldrWithKey computePrice 0.0 cart
   where computePrice product count acc = acc + ((price product) * fromIntegral count)

taxPrice :: Cart -> OfferAssociation -> Double -> Double
taxPrice cart offers taxRate = ((cartSum cart) - (discountPrice cart offers)) * (taxRate)

discountPrice :: Cart -> OfferAssociation -> Double
discountPrice cart offers = foldrWithKey computeDiscount 0.0 offers
  where computeDiscount product offer acc = acc + offer cart product

findProductQuantity :: Cart -> Product -> Int
findProductQuantity cart product = cart ! product

buy2Get1FreeOffer :: Offer
buy2Get1FreeOffer cart product = ((price product) * fromIntegral (floor (fromIntegral (findProductQuantity cart product) / 3)))

