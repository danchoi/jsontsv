{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Data.ByteString.Lazy as BL hiding (map, intersperse)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Attoparsec.Lazy as Atto hiding (Result)
import Data.Attoparsec.ByteString.Char8 (endOfLine, sepBy)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Scientific 

main = do
    x <- BL.getContents 
    let xs :: [Value]
        xs = decodeStream x
    let ks = [["title"],["rating"],["genres"]]
    mapM_ (print . evalToList ks) xs

decodeStream :: (FromJSON a) => BL.ByteString -> [a]
decodeStream = decodeWith (json `sepBy` endOfLine) fromJSON

decodeWith :: (FromJSON a) => Parser [Value] -> (Value -> Result a) -> BL.ByteString -> [a]
decodeWith p to s =
    case Atto.parse p s of
      Atto.Done _ vs -> catMaybes $ map f vs 
  where f v = (\x -> case x of 
                      Success a -> Just a 
                      _ -> Nothing) $ to v

type KeyPath = [Text]

evalToList :: [KeyPath] -> Value -> [Text]
evalToList ks v = map (flip evalToText v) ks

evalToText :: KeyPath -> Value -> Text
evalToText k v = valToText $ evalKeyPath k v

-- evaluates the a JS key path against a Value context to a leaf Value
evalKeyPath :: KeyPath -> Value -> Value
evalKeyPath [] x@(String _) = x
evalKeyPath [] x@Null = x
evalKeyPath [] x@(Number _) = x
evalKeyPath [] x@(Bool _) = x
evalKeyPath [] x@(Object _) = x
evalKeyPath [] x@(Array _) = x
evalKeyPath (key:ks) (Object s) = 
    case (HM.lookup key s) of
        Just (Array v) -> 
          let vs = V.toList v
              xs = intersperse "," $ map (evalToText ks) vs
          in String . mconcat $ xs
        Just x          -> evalKeyPath ks x
evalKeyPath _ _ = Null

valToText :: Value -> Text
valToText (String x) = x
valToText Null = ""
valToText (Bool True) = "true"
valToText (Bool False) = "false"
valToText (Number x) = 
    case floatingOrInteger x of
        Left float -> T.pack . show $ float
        Right int -> T.pack . show $ int
valToText (Object _) = "[Object]"

