module Utils.Aeson where

import Data.Aeson (FromJSON, eitherDecodeStrict)

encodeJson :: (FromJSON a) => Text -> Either Text a
encodeJson = first toText . eitherDecodeStrict . encodeUtf8
