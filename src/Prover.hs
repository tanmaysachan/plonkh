module Prover where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B

witnessFile :: FilePath
witnessFile = "src/samplewitness.json"

parsedJson :: IO (Maybe A.Object)
parsedJson = do
    json <- B.readFile witnessFile
    return (A.decode json :: Maybe A.Object)
