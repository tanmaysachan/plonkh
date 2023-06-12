module Circuit where

import qualified BabyJubJub as PrimeField (f)
import qualified Data.Map.Strict as Map
import qualified Polynomial as P

data InputType = Private | Public deriving (Show, Eq)
data GateType = Add | Mult | Const deriving (Show, Eq)

data Input = Input {
    getInputType :: InputType
  , identifier :: String
}

data Gate = Gate {
    getGateType :: GateType
  , left :: Input
  , right :: Input
  , out :: Input
  , c :: Integer
}

-- Selector rules

qL :: [Gate] -> [Integer]
qL = map (\x -> if getGateType x == Add then 1
                else if getGateType x == Const then 1
                else 0)

qR :: [Gate] -> [Integer]
qR = map (\x -> if getGateType x == Add then 1 else 0)

qO :: [Gate] -> [Integer]
qO = map (\x -> if getGateType x /= Const then -1 else 0)

qM :: [Gate] -> [Integer]
qM = map (\x -> if getGateType x == Mult then 1 else 0)

qC :: [Gate] -> [Integer]
qC = map (\x -> if getGateType x == Const then -c x else 0)

interpolate :: [Integer] -> P.LagrangePoly Double
interpolate xs = P.LagrangePoly $ P.lagrangeInterpAt $ zip [0,1..] (conv xs)
    where conv = map (\x -> fromIntegral x :: Double)

-- Witness assigments

lefts :: [Gate] -> [String]
lefts = map (identifier . left)

rights :: [Gate] -> [String]
rights = map (identifier . right)

outs :: [Gate] -> [String]
outs = map (identifier. out)

testingGates :: [Gate]
testingGates = [Gate Add (Input Public "x") (Input Public "y") (Input Private "z") 0, Gate Mult (Input Private "y") (Input Public "x0") (Input Private "a") 0]

