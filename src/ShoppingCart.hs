module ShoppingCart (
  addProductsToCart,
  totalPrice,
  taxPrice,
  findProductQuantity,
  Product(CartProduct),
  Cart(MyCart)
) where

data Product = CartProduct{
  name :: String,
  price :: Double
} deriving (Show, Eq)

data Cart = MyCart {
  products :: [Product],
  taxRate :: Double
} deriving (Show, Eq)

type AggregatedCost = Cart -> Double

addProductsToCart :: Cart -> Product -> Cart
addProductsToCart c p = MyCart  ((products c) ++ [p]) $ taxRate c

totalPrice :: AggregatedCost
totalPrice cart = total + (total * (taxRate cart)) where
    total = cartSum cart

cartSum :: AggregatedCost
cartSum cart = foldl computePrice 0.0 $ products cart
   where computePrice acc product = acc + (price product)

taxPrice :: AggregatedCost
taxPrice cart = (cartSum cart) * (taxRate cart)


findProductQuantity :: Cart -> Product -> Int
findProductQuantity cart product = length (filter (\p -> (name p) == (name product)) (products cart))
