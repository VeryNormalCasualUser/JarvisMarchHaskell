module Dims where
    
import GHC.Float (int2Double)

data Dim = ThreeD (Double, Double, Double) | TwoD (Double, Double) | D Double 
    deriving (Eq, Ord, Show)

getX:: Dim -> Double
getX (TwoD (a,b)) = a

getY :: Dim -> Double
getY (TwoD (a,b)) = b

get :: Dim -> Double
get (D a) = a


extract :: Dim -> (Double, Double)
extract (TwoD (a,b)) = (a,b)



instance Num Dim where

    (+) :: Dim -> Dim -> Dim
    (+) (TwoD (a,b)) (TwoD (c,d)) = TwoD (a+c, b+d)

    (-) :: Dim -> Dim -> Dim
    (-) (TwoD (a,b)) (TwoD (c,d)) = TwoD (a-c, b-d)

    (*) :: Dim -> Dim -> Dim
    (*) (D a) (D b)                 = D (a*b)
    (*) (TwoD (a,b)) (TwoD (c,d))   = D ((a*c) + (b*d))
    (*) (TwoD (a,b)) (D c)          = TwoD (a*c, b*c)
    (*) (D c) (TwoD (a,b))          = TwoD (a*c, b*c)

    abs :: Dim -> Dim
    abs (D a) = D (abs a)
    abs (TwoD (a, b))
        |  a == 0   &&  b == 0   = TwoD(a, b)
        | (a < 0)   && (b < 0)   = TwoD(-a,-b)
        | (a < 0)   && (b >= 0)  = TwoD(-a, -b)
        | (a >= 0)  && (b < 0)   = TwoD(-a, -b)
        | (a >= 0)  && (b >= 0)  = TwoD(a, b)



    signum :: Dim -> Dim
    signum (D a) = D (signum a)
    signum (TwoD (a,b))
        |  a == 0   &&  b == 0   = 0
        | (a < 0)   && (b < 0)   = -1
        | (a < 0)   && (b >= 0)  = -1
        | (a >= 0)  && (b < 0)   = -1
        | (a >= 0)  && (b >= 0)  = 1

    fromInteger :: Integer -> Dim
    fromInteger a = D (integer2Double a)
        
integer2Double :: Integer -> Double
integer2Double x = int2Double $ fromIntegral x   

class Vector a where
    (-->) :: a -> a -> a
    (><) :: a -> a -> a
    len :: a -> Double
    isScaled :: a -> a -> Bool

instance Vector Dim where

    (-->) :: Dim -> Dim -> Dim
    (-->) (TwoD (a,b)) (TwoD (c,d)) = TwoD (c-a, d-b)

    (><) :: Dim -> Dim -> Dim
    (><) (TwoD (a,b)) (TwoD (c,d)) = D (b*c - a*d)

    isScaled :: Dim -> Dim -> Bool
    isScaled (TwoD (0.0, 0.0)) _ = False
    isScaled _ (TwoD (0.0, 0.0)) = False
    isScaled (TwoD (y, 0.0)) (TwoD (x, 0.0)) = True
    isScaled (TwoD (0.0, y)) (TwoD (0.0, x)) = True
    isScaled (TwoD (a,b)) (TwoD (c,d)) = a/c == b/d || c/a == d/b

    len :: Dim -> Double
    len (TwoD (a,b))    = sqrt (a^2 + b^2)
    len (D a)           = abs a


isPerpendicular :: Dim -> Dim -> Bool
isPerpendicular u v = u*v == 0

isLeftTurn :: Dim -> Dim -> Bool
isLeftTurn u v  = get (u >< v) < 0.0

isRightTurn :: Dim -> Dim -> Bool
isRightTurn u v = get (u >< v) > 0.0

integerTupleToTwoD :: (Integer, Integer) -> Dim
integerTupleToTwoD (a,b) = TwoD (integer2Double a, integer2Double b)



maxLength :: Dim -> Dim -> Dim 
maxLength a b 
    |max (len a) (len b) == len a   = a
    |otherwise                      = b

minLength  :: Dim -> Dim -> Dim 
minLength  a b 
    |min (len a) (len b) == len a   = a
    |otherwise                      = b

