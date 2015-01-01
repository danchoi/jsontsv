{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where
import Data.Aeson
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.List (intersperse)
import qualified Data.List 
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Maybe (catMaybes)
import Control.Applicative
import Control.Monad (when)
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
import Data.String.QQ 

data Options = Options { 
    jsonExpr :: String
  , arrayDelim :: String
  , outputMode :: OutputMode
  , showHeader :: Bool
  , debugKeyPaths :: Bool
  } deriving Show

data OutputMode = TSVOutput { delimiter :: String } | CSVOutput deriving (Show)

parseOpts :: O.Parser Options
parseOpts = Options 
  <$> O.argument O.str (O.metavar "FIELDS")
  <*> O.strOption (O.metavar "DELIM" <> O.value "," <> O.short 'a' <> O.help "Concatentated array elem delimiter. Defaults to comma.")
  <*> (parseCSVMode <|> parseTSVMode)
  <*> O.flag False True (O.short 'H' <> O.help "Include headers")
  <*> O.switch (O.long "debug" <> O.help "Debug keypaths")

parseCSVMode = O.flag' CSVOutput (O.short 'c' <> O.long "csv" <> O.help "Output CSV")

parseTSVMode = TSVOutput 
    <$> (O.strOption 
          (O.metavar "DELIM" <> O.value "\t" <> O.short 'd' <> O.help "Output field delimiter. Defaults to tab."))

opts = O.info (O.helper <*> parseOpts)
          (O.fullDesc 
            <> O.progDesc [s| Transform JSON objects to TSV.  
                    On STDIN provide an input stream of whitespace-separated JSON objects. |]
            <> O.header "jsontsv"
            <> O.footer "See https://github.com/danchoi/jsontsv for more information.")

main = do
  Options expr arrayDelim mode showHeaders debugKeyPaths <- O.execParser opts
  x <- BL.getContents 
  let xs :: [Value]
      xs = decodeStream x
      ks = parseKeyPath $ T.pack expr
      -- keypaths without alias info:
      ks' = [ks' | KeyPath ks' _ <- ks]
      arrayDelim' = T.pack arrayDelim
  when debugKeyPaths $
     Prelude.putStrLn $ "key Paths " ++ show ks
  when showHeaders $ do
    let hs = [case alias of 
                Just alias' -> T.unpack alias'
                Nothing -> expr  
              | (KeyPath _ alias, expr) <- Data.List.zip ks (words expr)] 
    case mode of 
      TSVOutput delim -> Prelude.putStrLn . Data.List.intercalate delim $ hs
      CSVOutput -> Prelude.putStrLn . CSV.printCSV $ [hs]
  case mode of 
    TSVOutput delim -> mapM_ (TL.putStrLn . B.toLazyText . evalToLineBuilder arrayDelim' delim ks') xs
    CSVOutput -> Prelude.putStrLn . CSV.printCSV $ map (map T.unpack . evalToList arrayDelim' ks') $  xs

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


-- | KeyPath may have an alias for the header output
data KeyPath = KeyPath [Key] (Maybe Text) deriving Show

data Key = Key Text | Index Int deriving (Eq, Show)

parseKeyPath :: Text -> [KeyPath]
parseKeyPath s = case AT.parseOnly pKeyPaths s of
    Left err -> error $ "Parse error " ++ err 
    Right res -> res

spaces = many1 AT.space

pKeyPaths :: AT.Parser [KeyPath]
pKeyPaths = pKeyPath `AT.sepBy` spaces

pKeyPath :: AT.Parser KeyPath
pKeyPath = KeyPath 
    <$> (AT.sepBy1 pKeyOrIndex (AT.takeWhile1 $ AT.inClass ".["))
    <*> (pAlias <|> pure Nothing)

-- | A column header alias is designated by : followed by alphanum string after keypath
pAlias :: AT.Parser (Maybe Text)
pAlias = do
    AT.char ':'
    Just <$> AT.takeWhile1 (AT.inClass "a-zA-Z0-9_-")

pKeyOrIndex :: AT.Parser Key
pKeyOrIndex = pIndex <|> pKey

pKey = Key <$> AT.takeWhile1 (AT.notInClass " .[:")

pIndex = Index <$> AT.decimal <* AT.char ']'

evalToLineBuilder :: Text -> String -> [[Key]] -> Value -> B.Builder 
evalToLineBuilder arrayDelim delim ks v = 
    mconcat $ intersperse (B.fromText . T.pack $ delim) $  map (flip (evalToBuilder arrayDelim) v) ks

type ArrayDelimiter = Text

evalToList :: Text -> [[Key]] -> Value -> [Text]
evalToList arrayDelim ks v = map (flip (evalToText arrayDelim) v) ks

evalToBuilder :: ArrayDelimiter -> [Key] -> Value -> B.Builder
evalToBuilder d k v = valToBuilder $ evalKeyPath d k v

evalToText :: ArrayDelimiter -> [Key] -> Value -> Text
evalToText d k v = valToText $ evalKeyPath d k v

-- evaluates the a JS key path against a Value context to a leaf Value
evalKeyPath :: ArrayDelimiter -> [Key] -> Value -> Value
evalKeyPath d [] x@(String _) = x
evalKeyPath d [] x@Null = x
evalKeyPath d [] x@(Number _) = x
evalKeyPath d [] x@(Bool _) = x
evalKeyPath d [] x@(Object _) = x
evalKeyPath d [] x@(Array v) = 
          let vs = V.toList v
              xs = intersperse d $ map (evalToText d []) vs
          in String . mconcat $ xs
evalKeyPath d (Key key:ks) (Object s) = 
    case (HM.lookup key s) of
        Just x          -> evalKeyPath d ks x
        Nothing -> Null
evalKeyPath d (Index idx:ks) (Array v) = 
      let e = (V.!?) v idx
      in case e of 
        Just e' -> evalKeyPath d ks e'
        Nothing -> Null
-- traverse array elements with additional keys
evalKeyPath d ks@(Key key:_) (Array v) = 
      let vs = V.toList v
      in String . mconcat . intersperse d $ map (evalToText d ks) vs
evalKeyPath _ ((Index _):_) _ = Null
evalKeyPath _ _ _ = Null

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

