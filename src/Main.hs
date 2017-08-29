{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import CmdArgs (CmdArgs (..), parseCmdArgs)
import Network (FetchError (..), fetchOffer)
import Offer (dnsFromDhcpOffer, parseDhcpOffer)
import ResolvConf (editResolvConf, ResolvConfError (..))

import Control.Lens (over, _Left)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.State (evalState, state)
import Control.Monad.Trans (lift)
import Data.Word (Word8)
import System.Exit (ExitCode (..), exitWith, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Random (random, getStdGen)

data Error
  = FetchError FetchError
  | ParsingOfferError String
  | ResolvConfError ResolvConfError

showError :: Error -> (Int, String)
showError = \case
  FetchError (ListeningError e) -> (1, "Listening error: " ++ show e)
  FetchError (SendingError e) -> (2, "Sending error: " ++ show e)
  FetchError TimeoutError -> (3, "Timeout: giving up.")
  ParsingOfferError msg -> (4, "Parsing response failed: " ++ msg)
  ResolvConfError (ParsingError msg) -> (5, "Parsing resolv.conf failed: " ++ msg)
  ResolvConfError (ReadingError exc) -> (6, "Read error: " ++ show exc)
  ResolvConfError (WritingError exc) -> (6, "Write error: " ++ show exc)

getXid :: IO [Word8]
getXid =
  let rndXid = sequence $ replicate 4 (state random)
  in evalState rndXid <$> getStdGen

main :: IO ()
main = do
  args <- parseCmdArgs
  xid <- getXid
  res <- runExceptT $ do
    response <- ExceptT $ over _Left FetchError <$> fetchOffer xid
    offer <- either (throwError . ParsingOfferError) return $ parseDhcpOffer xid response
    dnss <- case dnsFromDhcpOffer offer of
      Nothing -> throwError $
        ParsingOfferError "Server response contains malformed DNS segment"
      Just ds -> return ds
    case args of
      PrintDNSs -> lift $ mapM_ print dnss
      UpdateFile file -> ExceptT . fmap (over _Left ResolvConfError) $
        editResolvConf file dnss
  case res of
    Left (showError -> (exitCode, msg)) -> do
      hPutStrLn stderr msg
      exitWith (ExitFailure exitCode)
    Right _ -> exitSuccess

