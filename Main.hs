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
  wordList <- T.readFile fp
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
main = return ()