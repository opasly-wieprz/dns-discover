{-# LANGUAGE ViewPatterns #-}

module Offer (DHCPOffer (..), parseDhcpOffer, dnsFromDhcpOffer)
where

import Data.Attoparsec.ByteString (anyWord8)
import qualified Data.Attoparsec.ByteString as AB (parseOnly, string)
import Data.Attoparsec.Combinator (count)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (pack)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M (empty, insert, lookup)
import Data.Word (Word8)

import IP (IP (..))

data DHCPOffer = DHCPOffer {
  clientAddress :: IP,
  serverAddress :: IP,
  dhcpOptions :: Map Word8 [Word8]
}

parseDhcpOffer :: [Word8] -> ByteString -> Either String DHCPOffer
parseDhcpOffer xid response = AB.parseOnly parser response
  where
    parser = do
      AB.string $ BS.pack [0x02]
      AB.string $ BS.pack [0x01]
      AB.string $ BS.pack [0x06]
      AB.string $ BS.pack [0x00]
      AB.string $ BS.pack xid
      AB.string $ BS.pack [0x00, 0x00]
      AB.string $ BS.pack [0x80, 0x00]
      AB.string $ BS.pack [0x00, 0x00, 0x00, 0x00]
      yiaddr <- ip
      siaddr <- ip
      AB.string $ BS.pack [0x00, 0x00, 0x00, 0x00]
      sequence (replicate 16 anyWord8) -- ignoring client's physical address
      AB.string $ BS.pack (replicate 192 0x00)
      AB.string $ BS.pack [0x63, 0x82, 0x53, 0x63]
      opts <- scanDhcpOptions M.empty
      return $ DHCPOffer yiaddr siaddr opts
      where
        ip = IP <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8
        dhcpOption = do
          len <- anyWord8
          count (fromIntegral len) anyWord8
        scanDhcpOptions options = do
          tag <- anyWord8
          case tag of
            0x00 -> scanDhcpOptions options -- padding - carry on
            0xff -> return options          -- endmark - finish
            _ -> do
              opt <- dhcpOption
              scanDhcpOptions (M.insert tag opt options)

dnsFromDhcpOffer :: DHCPOffer -> Maybe [IP]
dnsFromDhcpOffer (dhcpOptions -> options) = case M.lookup 0x06 options of
  Nothing -> Just []
  Just bs -> traverse f (chunksOf 4 bs)
    where
      f [a, b, c, d] = Just (IP a b c d)
      f _ = Nothing

