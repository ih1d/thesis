module Main (main) where

data Forward a = Forward
  { val :: a -> a
  , deriv :: a -> a
  }

class Sym repr where
  lit :: Num a => a -> repr a
  var :: Num a => repr a
  add :: Num a => repr a -> repr a -> repr a
  mul :: Num a => repr a -> repr a -> repr a

instance Sym Forward where
  lit x = Forward (const x) (const 0)
  var = Forward id (const 1)
  add a b = Forward (\x -> val a x + val b x) (\x -> deriv a x + deriv b x)
  mul a b = Forward (\x -> val a x * deriv b x) (\x -> val a x * deriv b x + deriv a x * val b x)

main :: IO ()
main = print 1
