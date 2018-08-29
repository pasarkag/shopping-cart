module ShoppingCart (
  addProductsToCart,
  totalPrice,
  taxPrice,
  findProductQuantity,
  Product(CartProduct),
  Cart
) where

data Product = CartProduct{
  name :: String,
  price :: Double
} deriving (Show, Eq)

type Cart = [Product]

addProductsToCart :: Cart -> Product -> Cart
addProductsToCart c p =  c ++ [p]

totalPrice :: Cart -> Double -> Double
totalPrice cart taxRate = total + (total * taxRate) where
    total = cartSum cart

cartSum :: Cart -> Double
cartSum cart = foldl computePrice 0.0 $ cart
   where computePrice acc product = acc + (price product)

taxPrice :: Cart -> Double -> Double
taxPrice cart taxRate = (cartSum cart) * (taxRate)

findProductQuantity :: Cart -> Product -> Int
findProductQuantity cart product = length (filter (\p -> (name p) == (name product)) (cart))
