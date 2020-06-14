module Main where

import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (chr)
import           Data.Either                (partitionEithers)
import           Data.List.Split            (chunksOf)
import           Data.Void
import           Data.Word

import           Control.Applicative        (many)
import           Control.Monad              (replicateM)

import           Text.Printf

import           System.Environment         (getArgs)

import           Replace.Megaparsec
import           Text.Megaparsec            hiding (State, chunk, many, parse)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void String

data Loc = Loc
  { _deg :: Float
  , _min :: Float
  , _sec :: Float
  } deriving (Show, Eq, Ord)

data Coord = Coord
  { _lat :: Loc
  , _lon :: Loc
  } deriving (Show, Eq, Ord)

number :: Integral a => Parser a
number = signed (return ()) decimal

digits :: Int -> Parser String
digits n = replicateM n digitChar

parseLoc :: Parser Loc
parseLoc = do
  deg <- fromIntegral <$> (number :: Parser Int)
  space
  mins <- read <$> digits 2
  sec <- fromIntegral <$> (number :: Parser Int)
  return $ Loc deg mins ((sec / 100.0) * 60.0)

parseCoord :: Parser Coord
parseCoord =
  Coord <$> between "AN " crlf parseLoc <*> between "BW " crlf parseLoc

parseString ::
     (Monad m, Stream s, ShowErrorComponent e) => s -> Parsec e s (m a) -> m a
parseString s p =
  case runParser p "" s of
    Left e  -> fail (errorBundlePretty e)
    Right a -> a

parseCoords :: [String] -> [Coord]
parseCoords = concatMap (\ s -> parseString s (many parseCoord))

parseFloats :: Get [Float]
parseFloats = many getFloatle

parseBitStream :: Parser [Either (Tokens String) String]
parseBitStream = sepCap parsePat
  where
    bin = count 1 binDigitChar
    parsePat =
      string ("11" ++ replicate 6 '0') <> bin <> bin <>
      string ("01" ++ replicate 6 '0') <>
      bin <>
      bin

parseFrames :: String -> [String]
parseFrames s =
  filter ((== 710) . length) $
  fst $ partitionEithers $ parseString s parseBitStream

decode :: String -> [Word8]
decode bs = map (toByte . take 8) $ chunksOf 10 bs
  where
    toByte bs' =
      sum $
      zipWith
        (\c s ->
           shift
             (if c == '0'
                then 0
                else 1)
             s)
        bs'
        [0 ..]

everyNth :: Int -> [b] -> [b]
everyNth n = map head . chunksOf n

toDeg :: Loc -> Float
toDeg loc = _deg loc + (_min loc / 60.0) + (_sec loc / 3600)

distKm :: Coord -> Coord -> Float
distKm ca cb =
  let toRad d = (toDeg d * pi) / 180
      lona = _lon ca
      lonb = _lon cb
      lata = _lat ca
      latb = _lat cb
      lonra = toRad lona
      lonrb = toRad lonb
      latra = toRad lata
      latrb = toRad latb
      a =
        (sin (latrb - latra) / 2) ** 2 +
        (cos latra * cos latrb * (sin (lonrb - lonra) / 2) ** 2)
      c = 2 * atan2 (sqrt a) (sqrt (1 - a))
      r = 6371
   in r * c

clean :: [Coord] -> [Coord]
clean cs = fmap snd $ filter (\(c1, c2) -> distKm c1 c2 < 0.1) $ zip (tail cs) cs

toKML :: FilePath -> [Coord] -> IO ()
toKML fp cs = do
  let ln =
        zipWith
          (\n c ->
             printf
               "<Placemark><TimeStamp><when>%d</when></TimeStamp><Point><coordinates>%f,%f,0</coordinates></Point></Placemark>\n"
               n
               (-toDeg (_lon c))
               (toDeg (_lat c)))
          ([0 ..] :: [Word32])
          cs
  writeFile
    fp
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Helo</name>\n"
  mapM_ (appendFile fp) ln
  appendFile fp "</Document></kml>\n"

main :: IO ()
main = do
  args <- getArgs
  let fn = head args
  bs <- BL.readFile fn
  let floats = runGet parseFloats bs
  let bits :: String =
        everyNth 2 $
        fmap
          (\v ->
             if v > 0
               then '0'
               else '1')
        floats
  let frames = decode <$> parseFrames bits
      ln = fmap (drop 2 . fmap (chr . fromIntegral)) frames
      coords = clean $ parseCoords ln
  toKML "output.kml" coords
