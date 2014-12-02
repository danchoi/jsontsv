{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Data.ByteString.Lazy as BL hiding (map)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Attoparsec.Lazy as Atto hiding (Result)
import Data.Attoparsec.ByteString.Char8 (endOfLine, sepBy)
import qualified Data.HashMap.Lazy as HM
import Data.Scientific 

main = do
    x <- BL.getContents 
    let xs :: [Value]
        xs = decodeStream x
    let ks = ["title"]
    mapM_ (print . evalToString ks) xs

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

evalToString :: KeyPath -> Value -> String
evalToString ks v = valToString $ evalKeyPath ks v

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
        Just (Array vs) -> String "[Array]"
        Just x          -> evalKeyPath ks x
evalKeyPath _ _ = Null

valToString :: Value -> String
valToString (String x) = T.unpack x
valToString Null = ""
valToString (Bool True) = "true"
valToString (Bool False) = "false"
valToString (Number x) = 
    case floatingOrInteger x of
        Left float -> show float
        Right int -> show int
valToString x = debugJSON x

debugJSON = B.unpack . encode 

