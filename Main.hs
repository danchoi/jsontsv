{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Maybe (catMaybes)
import Control.Applicative
import Data.ByteString.Lazy as BL hiding (map, intersperse)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Attoparsec.Lazy as Atto hiding (Result)
import Data.Attoparsec.ByteString.Char8 (endOfLine, sepBy)
import qualified Data.Attoparsec.Text as AT
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Scientific 
import System.Environment (getArgs)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import qualified Options.Applicative as O
import qualified Text.CSV as CSV

data Options = Options { jsonExpr :: String, outputMode :: OutputMode } deriving Show

data OutputMode = TSVOutput { delimiter :: String } | CSVOutput deriving (Show)

parseOpts :: O.Parser Options
parseOpts = Options 
  <$> O.argument O.str (O.metavar "FIELDS")
  <*> ((O.flag' CSVOutput (O.short 'c' <> O.long "csv" <> O.help "output CSV"))
       <|> (TSVOutput <$> (O.strOption (O.metavar "DELIM" <> O.value "\t" <> O.short 'd' <> O.help "output field delimiter. Defaults to tab"))))

opts = O.info (O.helper <*> parseOpts)
          (O.fullDesc <> O.progDesc "Transform JSON objects to TSV" <> O.header "jsontsv")

main = do
  Options expr mode <- O.execParser opts
  x <- BL.getContents 
  let xs :: [Value]
      xs = decodeStream x
  let ks' = parseKeyPath $ T.pack expr
  -- Prelude.putStrLn $ "key Paths " ++ show ks'
  case mode of 
    TSVOutput delim -> mapM_ (TL.putStrLn . B.toLazyText . evalToLineBuilder delim ks') xs
    CSVOutput -> Prelude.putStrLn . CSV.printCSV $ map (map T.unpack . evalToList ks') $  xs

decodeStream :: (FromJSON a) => BL.ByteString -> [a]
decodeStream bs = case decodeWith json bs of
    (Just x, xs) | xs == mempty -> [x]
    (Just x, xs) -> x:(decodeStream xs)
    (Nothing, _) -> []

decodeWith :: (FromJSON a) => Parser Value -> BL.ByteString -> (Maybe a, BL.ByteString)
decodeWith p s =
    case Atto.parse p s of
      Atto.Done r v -> f v r
      Atto.Fail _ _ _ -> (Nothing, mempty)
  where f v' r = (\x -> case x of 
                      Success a -> (Just a, r)
                      _ -> (Nothing, r)) $ fromJSON v'

parseKeyPath :: Text -> [KeyPath]
parseKeyPath s = case AT.parseOnly pKeyPaths s of
    Left err -> error $ "Parse error " ++ err 
    Right res -> res

spaces = many1 AT.space

pKeyPaths :: AT.Parser [KeyPath]
pKeyPaths = pKeyPath `AT.sepBy` spaces

pKeyPath :: AT.Parser KeyPath
pKeyPath = AT.sepBy1 pKeyOrIndex (AT.takeWhile1 $ AT.inClass ".[")

pKeyOrIndex = pIndex <|> pKey

pKey = Key <$> AT.takeWhile1 (AT.notInClass " .[")

pIndex = Index <$> AT.decimal <* AT.char ']'

type KeyPath = [Key]
data Key = Key Text | Index Int deriving (Eq, Show)

evalToLineBuilder :: String -> [KeyPath] -> Value -> B.Builder 
evalToLineBuilder delim ks v = mconcat $ intersperse (B.fromText . T.pack $ delim) $  map (flip evalToBuilder v) ks

evalToList :: [KeyPath] -> Value -> [Text]
evalToList ks v = map (flip evalToText v) ks

evalToBuilder :: KeyPath -> Value -> B.Builder
evalToBuilder k v = valToBuilder $ evalKeyPath k v

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
-- traverse array elements with additional keys
evalKeyPath ks@(Key key:_) (Array v) = 
      let vs = V.toList v
      in String . mconcat . intersperse "," $ map (evalToText ks) vs
evalKeyPath ((Index _):_) _ = Null
evalKeyPath _ _ = Null

valToBuilder :: Value -> B.Builder
valToBuilder (String x) = B.fromText x
valToBuilder Null = B.fromText "null"
valToBuilder (Bool True) = B.fromText "t"
valToBuilder (Bool False) = B.fromText "f"
valToBuilder (Number x) = 
    case floatingOrInteger x of
        Left float -> B.realFloat float
        Right int -> B.decimal int
valToBuilder (Object _) = B.fromText "[Object]"

valToText :: Value -> Text
valToText (String x) = x
valToText Null = "null"
valToText (Bool True) = "t"
valToText (Bool False) = "f"
valToText (Number x) = 
    case floatingOrInteger x of
        Left float -> T.pack . show $ float
        Right int -> T.pack . show $ int
valToText (Object _) = "[Object]"

