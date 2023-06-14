module Circuit where

import Util
import qualified BabyJubJub as PrimeField (f)
import qualified Data.Map.Strict as M
import qualified Polynomial as P
import qualified Data.List as L

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

type Circuit = [Gate]

-- Number of gates determined without circuit
numGates :: Integer
numGates = 3

-- Typecasting purposes
numGates' :: Int
numGates' = fromIntegral numGates :: Int

-- Selector rules
qL :: Circuit -> [Integer]
qL = map (\x -> if getGateType x == Add then 1
                else if getGateType x == Const then 1
                else 0)

qR :: Circuit -> [Integer]
qR = map (\x -> if getGateType x == Add then 1 else 0)

qO :: Circuit -> [Integer]
qO = map (\x -> if getGateType x /= Const then -1 else 0)

qM :: Circuit -> [Integer]
qM = map (\x -> if getGateType x == Mult then 1 else 0)

qC :: Circuit -> [Integer]
qC = map (\x -> if getGateType x == Const then -c x else 0)

-- Witness assigments
lefts :: Circuit -> [String]
lefts = map (identifier . left)

rights :: Circuit -> [String]
rights = map (identifier . right)

outs :: Circuit -> [String]
outs = map (identifier. out)

-- Copy constraints implementation
-- TODO: Domain needs to be powers of unity.

-- numGates needs to be independent of the circuit
cosetA :: [Integer]
cosetA = [0..(numGates - 1)]

cosetB :: [Integer]
cosetB = [numGates..(2 * numGates - 1)]

cosetC :: [Integer]
cosetC = [(2 * numGates)..(3 * numGates - 1)]

newDomains :: Circuit -> [Integer]
newDomains c = map (snd) (permuteDomain witnessNames cosets)
    where witnessNames = (lefts c) ++ (rights c) ++ (outs c)
          cosets = cosetA ++ cosetB ++ cosetC

sigmaA :: Circuit -> [Integer]
sigmaA c = take numGates' (newDomains c)

sigmaB :: Circuit -> [Integer]
sigmaB c = take numGates' (drop numGates' (newDomains c))

sigmaC :: Circuit -> [Integer]
sigmaC c = take numGates' (drop (2 * numGates') (newDomains c))


