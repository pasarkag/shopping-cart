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

import Data.Map (Map, (!), foldrWithKey)
import qualified Data.Map as Map

data Product = CartProduct{
  name :: String,
  price :: Double
} deriving (Show, Eq, Ord)

type Cart = [Product]

type Offer = Int -> Int

addProductsToCart :: Cart -> Product -> Cart
addProductsToCart c p =  c ++ [p]

totalPrice :: Cart -> Double -> Double
totalPrice cart taxRate = total + (total * taxRate) where
    total = cartSum cart

cartSum :: Cart -> Double
cartSum cart = foldl computePrice 0.0 cart
   where computePrice acc product = acc + (price product)

taxPrice :: Cart -> Double -> Double
taxPrice cart taxRate = (cartSum cart) * (taxRate)

discountPrice :: Cart -> (Map Product Offer) -> Double
discountPrice cart offers = foldrWithKey computeDiscount 0.0 offers
  where computeDiscount product offer acc = acc + ((price product) * fromIntegral (offer (findProductQuantity cart product)))

findProductQuantity :: Cart -> Product -> Int
findProductQuantity cart product = length (filter (\p -> (name p) == (name product)) (cart))

buy2Get1FreeOffer :: Offer
buy2Get1FreeOffer countOfProducts = floor ((fromIntegral countOfProducts) / 3)

