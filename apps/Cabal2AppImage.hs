module Main where

import           Prelude               hiding (head, (!!))

import           Data.List.Safe
import           Data.Maybe
import           Data.Set              (Set)
import qualified Data.Set              as S
import qualified Data.Text             as T
import qualified Data.Text.IO          as T

import           Control.Monad
import           Control.Monad.Extra   (unfoldM)

import           System.Directory
import           System.FilePath.Posix
import           System.Process

appName :: String
appName = "soapy-sdr"

appDir :: String
appDir = appName ++ ".AppDir"

addApps :: [String]
addApps = ["helidecode"]

strip :: String -> IO ()
strip fp = callProcess "strip" [fp]

copyLib :: String -> String -> IO ()
copyLib src dest = do
  putStrLn $ "Copying " ++ src ++ " to " ++ dest
  let sd = takeDirectory src
  ls <- listDirectory sd
  let fn = T.pack $ takeFileName src
      s = (\a -> sd ++ "/" ++ a) <$> filter (T.isInfixOf fn . T.pack) ls
  mapM_ (\a -> callProcess "cp" ["-d", a, takeDirectory dest]) s

copy :: String -> String -> IO ()
copy src dst = callProcess "cp" [src, dst]

cleanUp :: [T.Text] -> [T.Text]
cleanUp =
  filter
    (\a ->
       let s = T.unpack a
        in isAbsolute s || not (null s))

ldd :: String -> IO (Set T.Text)
ldd execName = do
  out <- T.pack <$> readProcess "ldd" [execName] []
  let p = entry . T.split (== ' ') . T.strip <$> T.lines out
  return $ S.fromList $ cleanUp $ fromJust <$> filter isJust p
  where
    entry ts = do
      name <- ts !! (1 :: Int)
      if name == "=>"
        then ts !! (2 :: Int)
        else Nothing

expand :: Set T.Text -> IO (Set T.Text)
expand = foldM go S.empty
  where
    go s l = S.union s <$> ldd (T.unpack l)

follow :: Set T.Text -> IO (Set T.Text)
follow s = do
  s' <- S.unions <$> unfoldM go s
  return (S.union s s')
  where
    go t = do
      t' <- expand t
      if S.null t'
        then return Nothing
        else return (Just (t', t'))

excludeList :: IO (Set String)
excludeList = do
  el <- fmap T.strip . T.lines . T.pack <$> readFile "excludelist.txt"
  return $
    S.fromList $
    T.unpack . fromJust . head . T.splitOn " " <$>
    filter (not . (\a -> T.isPrefixOf "#" a || T.null a)) el

additionalLibs :: IO (Set T.Text)
additionalLibs = do
  l1 <- T.pack <$> readProcess "locate" ["librtlsdr.so"] []
  l2 <- T.pack <$> readProcess "locate" ["librtlsdrSupport.so"] []
  return . S.fromList $ cleanUp $ fmap T.strip (T.lines l1 ++ T.lines l2)

desktopFile :: IO ()
desktopFile = do
  writeFile (appDir ++ "/" ++ appName ++ ".desktop") $
    unlines
      [ "[Desktop Entry]"
      , "Type=Application"
      , "Name=Soapy SDR"
      , "Comment=I/Q recorder and processor using SoapySDR as backend" 
      , "Exec=" ++ appName
      , "Icon=" ++ appName
      , "Categories=Utility;"
      , "Terminal=true"
      ]
  copy (appName ++ ".png") (appDir ++ "/" ++ appName ++ ".png")

appRunFile :: Set T.Text -> IO ()
appRunFile ls = do
  let dirs = S.map (T.pack . takeDirectory . T.unpack) ls
      md = S.elemAt 0 $ S.filter (T.isInfixOf "modules") dirs
      p =
        T.concat $
        intersperse ":" $ S.toList $ S.map ("${HERE}" `T.append`) dirs
  f <- T.replace "%libs%" p <$> T.readFile "AppRun"
  T.writeFile
    (appDir ++ "/AppRun")
    (T.replace "%modules%" ("${HERE}" `T.append` md) f)
  callProcess "chmod" ["+x", appDir ++ "/AppRun"]

main :: IO ()
main = do
  hd <- getHomeDirectory
  ex <- excludeList
  ps <- ldd (hd ++ "/.cabal/bin/" ++ appName) >>= follow
  ads <- mapM (ldd >=> follow) $ fmap (\n -> hd ++ "/.cabal/bin/" ++ n) addApps
  as <- additionalLibs >>= follow
  let libs =
        S.filter
          (\a -> not $ S.member (takeFileName $ T.unpack a) ex)
          (S.unions $ ps : as : ads)
  callProcess "rm" ["-Rf", appDir]
  createDirectory appDir
  createDirectory $ appDir ++ "/bin"
  mapM_
    (\n -> callProcess "cp" [hd ++ "/.cabal/bin/" ++ n, appDir ++ "/bin/" ++ n])
    (appName : addApps)
  mapM_ (strip . (\n -> appDir ++ "/bin/" ++ n)) (appName : addApps)
  mapM_
    (\fp ->
       createDirectoryIfMissing True $ appDir ++ takeDirectory (T.unpack fp))
    libs
  mapM_ (\fp -> copyLib (T.unpack fp) (appDir ++ T.unpack fp)) libs
  mapM_ (\fp -> strip $ appDir ++ T.unpack fp) libs
  desktopFile
  appRunFile libs
