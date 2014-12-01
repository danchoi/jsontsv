module Main where
import Data.Aeson
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.ByteString.Lazy as BL hiding (map)
import Data.Attoparsec.Lazy as Atto hiding (Result)
import Data.Attoparsec.ByteString.Char8 (endOfLine, sepBy)

main = do
  x <- BL.getContents 
  let xs :: [M.Map Text Value]
      xs = decodeStream x
  mapM_ print xs

decodeStream :: (FromJSON a) => BL.ByteString -> [a]
decodeStream = decodeWith (json `sepBy` endOfLine) fromJSON

decodeWith :: (FromJSON a) 
           => Parser [Value] -> (Value -> Result a) -> BL.ByteString -> [a]
decodeWith p to s =
    case Atto.parse p s of
      Atto.Done _ vs -> catMaybes $ map f vs 
  where f v = (\x -> case x of 
                      Success a -> Just a 
                      _ -> Nothing) $ to v

-- fromJSON :: FromJSON a => Value -> Result a Source


