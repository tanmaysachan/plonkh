module Util where

import qualified Data.Map.Strict as M
import qualified Data.List as L

permuteDomain :: [String] -> [Integer] -> [(String, Integer)]
permuteDomain = permuteDomain' M.empty
    where permuteDomain' _ [] [] = []
          permuteDomain' m (g:gs) (d:ds) = case L.findIndex (== g) gs of
            Just i -> do
                -- Index to permute to
                let d' = ds !! i
                -- Keep location of the first occurance in map
                let m' = M.insertWith (flip const) g d m
                (g, d') : permuteDomain' m' gs ds
            Nothing -> case M.lookup g m of
                Just d' -> (g, d') : permuteDomain' m gs ds
                Nothing -> (g, d) : permuteDomain' m gs ds
