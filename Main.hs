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
import Control.Applicative
import Data.ByteString.Lazy as BL hiding (map, intersperse)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Attoparsec.Lazy as Atto hiding (Result)
import Data.Attoparsec.ByteString.Char8 (endOfLine, sepBy)
import qualified Data.Attoparsec.Text as AT
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Scientific 
import System.Environment (getArgs)

main = do
    x <- BL.getContents 
    let xs :: [Value]
        xs = decodeStream x
    (ks:_) <- getArgs 
    let ks' = parseKeyPath $ T.pack ks
    Prelude.putStrLn $ "key Paths " ++ show ks'
    mapM_ (print . evalToList ks') xs

decodeStream :: (FromJSON a) => BL.ByteString -> [a]
decodeStream = decodeWith (json `sepBy` endOfLine) fromJSON

decodeWith :: (FromJSON a) => Parser [Value] -> (Value -> Result a) -> BL.ByteString -> [a]
decodeWith p to s =
    case Atto.parse p s of
      Atto.Done _ vs -> catMaybes $ map f vs 
  where f v = (\x -> case x of 
                      Success a -> Just a 
                      _ -> Nothing) $ to v

parseKeyPath :: Text -> [KeyPath]
parseKeyPath s = case AT.parseOnly pKeyPaths s of
    Left err -> error $ "Parse error " ++ err 
    Right res -> res

spaces = many1 AT.space

pKeyPaths :: AT.Parser [KeyPath]
pKeyPaths = pKeyPath `AT.sepBy` spaces

pKeyPath :: AT.Parser KeyPath
pKeyPath = AT.sepBy1 pKeyOrIndex (AT.takeWhile1 $ AT.inClass ".[]")

pKeyOrIndex = pIndex <|> pKey

pKey = Key <$> AT.takeWhile1 (AT.notInClass " .[")

pIndex = Index <$> AT.decimal 

type KeyPath = [Key]
data Key = Key Text | Index Int deriving (Eq, Show)

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
evalKeyPath [] x@(Array v) = 
          let vs = V.toList v
              xs = intersperse "," $ map (evalToText []) vs
          in String . mconcat $ xs
evalKeyPath (Key key:ks) (Object s) = 
    case (HM.lookup key s) of
        Just x          -> evalKeyPath ks x
        Nothing -> Null
evalKeyPath (Index idx:ks) (Array v) = 
      let e = (V.!?) v idx
      in case e of 
        Just e' -> evalKeyPath ks e'
        Nothing -> Null
evalKeyPath ((Index _):_) _ = Null
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

