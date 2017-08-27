{-# LANGUAGE OverloadedStrings #-}

module ResolvConf (editResolvConf, ResolvConfError (..))
where

import Control.Applicative ((<|>))
import Control.Exception (handle, SomeException)
import Control.Lens (_Left, over)
import Control.Monad (join)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Attoparsec.Combinator (sepBy)
import Data.Attoparsec.Text (char, decimal, isEndOfLine, isHorizontalSpace)
import qualified Data.Attoparsec.Text as AT (
  endOfInput,
  parseOnly,
  satisfy,
  string,
  takeTill,
  takeWhile)
import Data.Functor (void)
import Data.List (foldl')
import Data.Semigroup ((<>))
import qualified Data.Text as T (Text, concat, pack)
import qualified Data.Text.IO as T (hGetContents)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.Builder as TLB (Builder, fromText, toLazyText)
import qualified Data.Text.Lazy.IO as TL (hPutStr)
import System.IO (Handle, IOMode (..), withFile)

import IP (IP (..))

data ResolvConfError
  = ParsingError String
  | ReadingError SomeException
  | WritingError SomeException

parseResolvConf :: T.Text -> Either ResolvConfError [Either T.Text T.Text]
parseResolvConf bs = over _Left ParsingError $ AT.parseOnly resolvConf bs
  where
    spaces = AT.takeWhile isHorizontalSpace
    ip = IP <$> (decimal <* char '.')
            <*> (decimal <* char '.')
            <*> (decimal <* char '.')
            <*> decimal
    nameserverLine = do
      s1 <- spaces
      n <- AT.string "nameserver"
      s2 <- spaces
      addr <- ip
      rest <- AT.takeTill isEndOfLine
      return $ T.concat [s1, n, s2, T.pack (show addr), rest]
    otherLine = AT.takeWhile (not . isEndOfLine)
    line = fmap Left nameserverLine <|> fmap Right otherLine
    eol = AT.satisfy isEndOfLine
    resolvConf = sepBy line eol <* (void eol <|> return ()) <* AT.endOfInput

mergeResolvConf :: [Either T.Text T.Text] -> [IP] -> TL.Text
mergeResolvConf resolvConf ips =
    TLB.toLazyText
  . fst
  . foldl' onlyThree (mempty, 0)
  $ map g ips ++ resolvConf
  where
    g ip = Left . T.pack $ "nameserver " ++ show ip
    onlyThree :: (TLB.Builder, Int) -> Either T.Text T.Text -> (TLB.Builder, Int)
    onlyThree (acc, nsCount) (Right rest) =
      (acc <> TLB.fromText rest <> "\n", nsCount)
    onlyThree (acc, nsCount) (Left rest)
      | nsCount < 3 = (acc <> TLB.fromText rest <> "\n", nsCount + 1)
      | otherwise = (acc, nsCount)

withFile'
  :: (SomeException -> a)
  -> FilePath
  -> IOMode
  -> (Handle -> IO b)
  -> IO (Either a b)
withFile' f file mode io =
  over _Left f <$> handle (return . Left) (Right <$> withFile file mode io)

editResolvConf :: FilePath -> [IP] -> IO (Either ResolvConfError ())
editResolvConf file dnss = runExceptT $ do
  parsed <-
      ExceptT
    . fmap join
    . withFile' ReadingError file ReadMode
    $ fmap parseResolvConf . T.hGetContents
  let merged = mergeResolvConf parsed dnss
  ExceptT $ withFile' WritingError file WriteMode (\h -> TL.hPutStr h merged)

