{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
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
import Data.Maybe (catMaybes, listToMaybe)
import Control.Applicative
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BL hiding (map, intersperse)
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
  , optArrayDelim :: Text
  , outputMode :: OutputMode
  , showHeader :: Bool
  , nullString :: Text
  , optTrueString :: Text
  , optFalseString :: Text
  , optNewLineReplacement :: Char
  , debugKeyPaths :: Bool
  } deriving Show

data OutputMode = TSVOutput { delimiter :: String } | CSVOutput deriving (Show)

parseOpts :: O.Parser Options
parseOpts = Options 
  <$> O.argument O.str (O.metavar "FIELDS")
  <*> (T.pack 
        <$> O.strOption (O.metavar "DELIM" <> O.value "," <> O.short 'a' <> O.help "Concatentated array elem delimiter. Defaults to comma."))
  <*> (parseCSVMode <|> parseTSVMode)
  <*> O.flag False True (O.short 'H' <> O.help "Include headers")
  <*> (T.pack 
        <$> O.strOption (O.value "null" 
            <> O.short 'n' <> O.long "null-string"
            <> O.metavar "STRING" <> O.help "String to represent null value. Default: 'null'"))
  <*> (T.pack 
        <$> O.strOption (O.value "t" 
            <> O.short 't' <> O.long "true-string"
            <> O.metavar "STRING" <> O.help "String to represent boolean true. Default: 't'"))
  <*> (T.pack 
        <$> O.strOption (O.value "f" 
            <> O.short 'f' <> O.long "false-string"
            <> O.metavar "STRING" <> O.help "String to represent boolean false. Default: 'f'"))
  <*> O.option O.auto (O.value ' ' 
            <> O.short 'N' <> O.long "newline"
            <> O.metavar "STRING" <> O.help "String to replace newlines in field text. Default: ' '")
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
  Options{..} <- O.execParser opts
  x <- BL.getContents 
  let xs :: [Value]
      xs = decodeStream x
      ks = parseKeyPath $ T.pack jsonExpr
      -- keypaths without alias info:
      ks' = [ks' | KeyPath ks' _ <- ks]
  when debugKeyPaths $
     Prelude.putStrLn $ "key Paths " ++ show ks
  when showHeader $ do
    let hs = [case alias of 
                Just alias' -> T.unpack alias'
                Nothing -> jsonExpr  
              | (KeyPath _ alias, jsonExpr) <- Data.List.zip ks (words jsonExpr)] 
              -- Note `words` introduces a potential bug is quoted aliases are allowed
              -- See https://github.com/danchoi/jsonxlsx/commit/9aedb4bf97cfa8d5635edc4780bfbf9b79b6f2ec
    case outputMode of 
      TSVOutput delim -> Prelude.putStrLn . Data.List.intercalate delim $ hs
      CSVOutput -> Prelude.putStrLn . CSV.printCSV $ [hs]
  let config = Config {   
                  arrayDelim = optArrayDelim
                , nullValueString = nullString
                , trueString = optTrueString
                , falseString = optFalseString
                , newlineReplacement = optNewLineReplacement
                }
  case outputMode of 
    TSVOutput delim -> mapM_ (TL.putStrLn . B.toLazyText . evalToLineBuilder config delim ks') xs
    CSVOutput -> Prelude.putStrLn . CSV.printCSV $ map (map T.unpack . evalToList config ks') $  xs

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


data Config = Config {
    arrayDelim :: Text
  , nullValueString :: Text
  , trueString :: Text
  , falseString :: Text
  , newlineReplacement :: Char
  } deriving Show

-- | KeyPath may have an alias for the header output
data KeyPath = KeyPath [Key] (Maybe Text) deriving Show

{-
  Key represents an object (HashMap) key.
  Index represents a position in an Array
  TupleKey represents a Text key in an array of pairs that should be treated 
    as a map. E.g.
        [["dinner","fish"],["dessert","pie"]]
    This is useful in the case of Data.Aeson emits pair tuples:
        [("dinner", "fish"),("dessert","pie")]
-}
data Key = Key Text 
         | Index Int 
         | TupleKey Text deriving (Eq, Show)


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
pKeyOrIndex = 
    ((pTupleKey <|> pIndex) <* AT.char ']')
    <|> pKey

pIndex :: AT.Parser Key   
pIndex = Index <$> AT.decimal 

pTupleKey :: AT.Parser Key
pTupleKey = do 
    AT.char '"' 
    xs <- AT.takeWhile1 (AT.notInClass "\"")
    AT.char '"'
    return $ TupleKey xs

pKey :: AT.Parser Key
pKey = Key <$> AT.takeWhile1 (AT.notInClass " .[:")

evalToLineBuilder :: Config -> String -> [[Key]] -> Value -> B.Builder 
evalToLineBuilder config@Config{..} outDelim ks v = 
    mconcat $ intersperse (B.fromText . T.pack $ outDelim) $  map (flip (evalToBuilder config) v) ks

type ArrayDelimiter = Text

evalToList :: Config -> [[Key]] -> Value -> [Text]
evalToList c@Config{..} ks v = map (flip (evalToText c) v) ks

evalToBuilder :: Config -> [Key] -> Value -> B.Builder
evalToBuilder c k v = valToBuilder c $ evalKeyPath c k v

evalToText :: Config -> [Key] -> Value -> Text
evalToText c k v = valToText c $ evalKeyPath c k v

-- evaluates the a JS key path against a Value context to a leaf Value
evalKeyPath :: Config -> [Key] -> Value -> Value
evalKeyPath config [] x@(String _) = x
evalKeyPath config [] x@Null = x
evalKeyPath config [] x@(Number _) = x
evalKeyPath config [] x@(Bool _) = x
evalKeyPath config [] x@(Object _) = x
evalKeyPath config [] x@(Array v) | V.null v = Null
evalKeyPath config [] x@(Array v) = 
          let vs = V.toList v
              xs = intersperse (arrayDelim config) $ map (evalToText config []) vs
          in String . mconcat $ xs
evalKeyPath config (Key key:ks) (Object s) = 
    case (HM.lookup key s) of
        Just x          -> evalKeyPath config ks x
        Nothing -> Null
evalKeyPath config (Index idx:ks) (Array v) = 
      let e = (V.!?) v idx
      in case e of 
        Just e' -> evalKeyPath config ks e'
        Nothing -> Null
evalKeyPath config (TupleKey k:ks) (Array v) = 
      -- array must be an array of 2-tuples
      let vs :: [(Value, Value)]
          vs = [(V.head tuple, tuple V.! 1) | Array tuple <- V.toList v , V.length tuple == 2]
          e = listToMaybe [v' | (String k', v') <- vs, k' == k]
      in maybe Null (evalKeyPath config ks) e
-- traverse array elements with additional keys
-- if key is _, e.g. cast._[0] , traverse INTO each array object
-- e.g. "cast":[["Michael Caine",13473],["Demi Moore",65231],...
evalKeyPath config@Config{..} (Key "_":ks) (Array v) = 
      String . mconcat . intersperse arrayDelim $ map (evalToText config ks) (V.toList v)
-- traverse array elements with additional keys
evalKeyPath _ ks@(Key key:_) (Array v) | V.null v = Null
evalKeyPath config@Config{..} ks@(Key key:_) (Array v) = 
      let vs = V.toList v
      in String . mconcat . intersperse arrayDelim $ map (evalToText config ks) vs

evalKeyPath _ ((Index _):_) _ = Null
evalKeyPath _ _ _ = Null

valToBuilder :: Config -> Value -> B.Builder
valToBuilder Config{..} (String x) = B.fromText . T.map (replaceNewLine newlineReplacement) $ x
valToBuilder Config{..} Null = B.fromText nullValueString
valToBuilder Config{..} (Bool True) = B.fromText trueString
valToBuilder Config{..} (Bool False) = B.fromText falseString
valToBuilder _ (Number x) = 
    case floatingOrInteger x of
        Left float -> B.realFloat float
        Right int -> B.decimal int
valToBuilder _ (Object _) = B.fromText "[Object]"

valToText :: Config -> Value -> Text
valToText Config{..} (String x) = T.map (replaceNewLine newlineReplacement) x
valToText Config{..} Null = nullValueString
valToText Config{..} (Bool True) = trueString
valToText Config{..} (Bool False) = falseString
valToText _ (Number x) = 
    case floatingOrInteger x of
        Left float -> T.pack . show $ float
        Right int -> T.pack . show $ int
valToText _ (Object _) = "[Object]"

replaceNewLine :: Char -> (Char -> Char)
replaceNewLine x s | s `elem` ['\n', '\r'] = x
                   | otherwise             = s
    
