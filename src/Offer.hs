{-# LANGUAGE ViewPatterns #-}

module Offer (DHCPOffer (..), parseDhcpOffer, dnsFromDhcpOffer)
where

import Data.Attoparsec.ByteString ((<?>), anyWord8)
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
    parser = (<?> "DHCPOffer") $ do
      AB.string (BS.pack [0x02]) <?> "OP"
      AB.string (BS.pack [0x01]) <?> "HTYPE"
      AB.string (BS.pack [0x06]) <?> "HLEN"
      AB.string (BS.pack [0x00]) <?> "HOPS"
      AB.string (BS.pack xid) <?> "XID"
      AB.string (BS.pack [0x00, 0x00]) <?> "SECS"
      AB.string (BS.pack [0x80, 0x00]) <?> "FLAGS"
      AB.string (BS.pack [0x00, 0x00, 0x00, 0x00]) <?> "Client IP address"
      yiaddr <- ip <?> "Your IP address"
      siaddr <- ip <?> "Server IP address"
      AB.string (BS.pack [0x00, 0x00, 0x00, 0x00]) <?> "Gateway IP address"

      -- Ignoring client's physical address.
      sequence (replicate 16 anyWord8) <?> "Client hardware address"

      -- This field is documented as 192 bytes of zeros.
      -- However testing some servers showed that it is not
      -- always the case.
      sequence (replicate 192 anyWord8) <?> "BOOTP legacy"

      AB.string (BS.pack [0x63, 0x82, 0x53, 0x63]) <?> "Magic cookie"
      opts <- scanDhcpOptions M.empty
      return $ DHCPOffer yiaddr siaddr opts
      where
        ip = (<?> "IP address") $
          IP <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8
        dhcpOption = do
          len <- anyWord8
          count (fromIntegral len) anyWord8
        scanDhcpOptions options = (<?> "DHCP Options") $ do
          tag <- anyWord8
          case tag of
            0x00 -> scanDhcpOptions options -- padding - carry on
            0xff -> return options          -- endmark - finish
            _ -> do
              opt <- dhcpOption <?> "Option " ++ show tag
              scanDhcpOptions (M.insert tag opt options)

dnsFromDhcpOffer :: DHCPOffer -> Maybe [IP]
dnsFromDhcpOffer (dhcpOptions -> options) = case M.lookup 0x06 options of
  Nothing -> Just []
  Just bs -> traverse f (chunksOf 4 bs)
    where
      f [a, b, c, d] = Just (IP a b c d)
      f _ = Nothing

