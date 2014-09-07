module Main where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.Char (isLower)
import Data.Function (on)
import Data.List (foldl', sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import System.IO (hFlush, hPutStr, hPutStrLn, stdout, stderr)
import System.IO.Error (catchIOError, isEOFError, isDoesNotExistError,
                       isAlreadyInUseError, isPermissionError, ioeGetFileName)

type Bag = M.Map Char Int

delete :: Char -> Bag -> Maybe Bag
delete c b = case M.lookup c b of
  Nothing -> Nothing
  Just n | n == 1 -> Just $ M.delete c b
         | otherwise -> Just $ b & ix c -~ 1

toBag :: Text -> Bag
toBag = T.foldl' (\b c -> M.insertWith (+) c 1 b) M.empty

data WordRec = WordRec Bag Int Text
  deriving Show

instance NFData WordRec where
  rnf (WordRec b c w) = rnf b `seq` rnf c `seq` rnf w

bag :: Lens' WordRec Bag
bag f (WordRec b c w) = fmap (\b' -> WordRec b' c w) (f b)

count :: Lens' WordRec  Int
count f (WordRec b c w) = fmap (\c' -> WordRec b c' w) (f c)

getWord :: WordRec -> Maybe Text
getWord (WordRec _ 0 w) = Just w
getWord _ = Nothing

check :: Text -> Maybe WordRec
check t | T.all isLower t = Just $ WordRec (toBag t) (T.length t)t
        | otherwise = Nothing

importDict :: FilePath -> IO [WordRec]
importDict fp = do
  wordList <- T.readFile fp `catchIOError` \e -> case () of
    _ | isDoesNotExistError e -> hPutStr stderr fp >>
                                 hPutStr stderr " does not exist. Are you su" >>
                                 hPutStr stderr "re you typed the filename c" >>
                                 hPutStrLn stderr "orrectly?" >>
                                 exitFailure
    _ | isAlreadyInUseError e -> hPutStr stderr fp >>
                                 hPutStr stderr " is already in use. Is ther" >>
                                 hPutStr stderr "e another program using it?" >>
                                 hPutStrLn stderr "" >>
                                 exitFailure
    _ | isPermissionError   e -> hPutStr stderr "You do not have permission " >>
                                 hPutStr stderr "to open " >>
                                 hPutStr stderr fp >>
                                 hPutStr stderr ". Check with a system admin" >>
                                 hPutStrLn stderr "istrator." >>
                                 exitFailure
    _ -> ioError e
  return $!! mapMaybe check $ T.lines wordList

filterLetter :: Char -> Int -> [WordRec] -> [WordRec]
filterLetter k c wrs = do
  wr <- wrs
  case wr & bag %%~ delete k of
    Nothing -> guard (view count wr <= c) >> return wr
    Just wr' -> return (wr' & count -~ 1)

filterLetters :: [Char] -> Int -> [WordRec] -> [WordRec]
filterLetters ks c wrs = foldl' (\wrs' (k,r) -> filterLetter k r wrs') wrs $ zip ks [c,c-1..]

findBest :: [WordRec] -> [Text]
findBest = sortBy (compare `on` T.length) . mapMaybe getWord

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      hPutStrLn stderr "No dictionary loaded. Try in /usr/dict or /usr/share/dict"
      exitFailure
    else do
      dict <- fmap concat $ mapM importDict args
      mainLoop dict

mainLoop :: [WordRec] -> IO ()
mainLoop dict = do
  putStr "> "
  hFlush stdout
  resp <- catchIOError getLine $ \e ->
    if isEOFError e
    then do
      putChar '\n'
      exitSuccess
    else ioError e
  print $ findBest$ filterLetters resp (length resp) dict
  mainLoop dict
