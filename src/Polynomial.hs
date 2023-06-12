module Polynomial where

import qualified Data.List as L

newtype Poly a = Poly { coeffs :: [a] } deriving (Show)

-- Addition Op for Poly
(+^) :: (Num a) => Poly a -> Poly a -> Poly a
(+^) f g = Poly $ zipWith (+) (coeffs f) (coeffs g)

-- Equality for Poly
instance (Eq a) => Eq (Poly a) where
    (==) f g = coeffs f == coeffs g

-- Evaluation of p(x)
eval :: (Num a) => Poly a -> a -> a
eval p x = sum $ zipWith (*) (coeffs p) (zipWith (^) (repeat x) [0,1..])

-- Evaluation of Interpolated polynomial at x
-- (Probably very inefficient)
lagrangeInterpAt :: (Eq a, Fractional a) => [(a, a)] -> a -> a
lagrangeInterpAt points x = sum $ zipWith (*) ys (map calcProd xs)
    where
        xs = map fst points
        ys = map snd points
        calcProd xi = product $ map (\xj -> (x-xj)/(xi-xj)) (L.delete xi xs)

newtype LagrangePoly a = LagrangePoly { runPoly :: (a -> a) }
