{-# LANGUAGE LambdaCase #-}

module Network (FetchError (..), fetchOffer)
where

import Extended.Control.Concurrent.Async (raceCatch)

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (pack)
import Data.Word (Word8)
import Network.Socket (
  addrFlags,
  AddrInfo (..),
  AddrInfoFlag (..),
  addrSocketType,
  bind,
  defaultHints,
  getAddrInfo,
  isBound,
  setSocketOption,
  socket,
  SocketType (..),
  SocketOption (..))
import Network.Socket.ByteString (sendTo, recvFrom)

data FetchError
  = ListeningError SomeException
  | SendingError SomeException
  | TimeoutError

sendDhcpDnsRequest :: [Word8] -> IO ()
sendDhcpDnsRequest xid = do
  let hints = defaultHints { addrSocketType = Datagram }
  addrInfo <- head <$> getAddrInfo (Just hints) (Just "255.255.255.255") (Just "67")
  sock <- socket (addrFamily addrInfo)
                 (addrSocketType addrInfo)
                 (addrProtocol addrInfo)
  setSocketOption sock Broadcast 1
  sendTo sock msg (addrAddress addrInfo)
  return ()
  where
    msg = BS.pack . concat $ [
        [0x01],                    -- OP
        [0x01],                    -- HTYPE
        [0x06],                    -- HLEN
        [0x00],                    -- HOPS
        xid,
        [0x00, 0x00],              -- SECS
        [0x80, 0x00],              -- FLAGS
        [0x00, 0x00, 0x00, 0x00],  -- CIADDR
        [0x00, 0x00, 0x00, 0x00],  -- YIADDR
        [0x00, 0x00, 0x00, 0x00],  -- SIADDR
        [0x00, 0x00, 0x00, 0x00],  -- GIADDR
        [0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00, 0x00,
         0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], -- CHADDR (dummy MAC address)
        replicate 192 0x00,        -- 192 octets of zeros
        [0x63, 0x82, 0x53, 0x63],  -- magic cookie
        [0x35,                     -- DHCP Discover
         0x01,                     -- DHCP Discover length
         0x01],                    -- DHCP Discover request
        [0x37,                     -- Parameter request list
         0x01,                     -- parameter length
         0x06],                    -- Domain Name Server
        [0xff]                     -- Endmark
      ]

receiveDhcpOffer :: IO (IO ByteString, IO ())
receiveDhcpOffer = do
  let hints = defaultHints { addrSocketType = Datagram, addrFlags = [AI_PASSIVE] }
  addrInfo <- head <$> getAddrInfo (Just hints) Nothing (Just "68")
  sock <- socket (addrFamily addrInfo)
                 (addrSocketType addrInfo)
                 (addrProtocol addrInfo)
  setSocketOption sock ReuseAddr 1
  let listen = do
        bind sock (addrAddress addrInfo)
        fst <$> recvFrom sock 0x10000
      awaitBound = do
        bound <- isBound sock
        unless bound $ do
          threadDelay 200
          awaitBound
  return (listen, awaitBound)

interpretRace
  :: Either (Either SomeException ByteString) (Either SomeException ())
  -> Either FetchError ByteString
interpretRace = \case
  Left (Left listenException) -> Left (ListeningError listenException)
  Right (Left sendException) -> Left (SendingError sendException)
  Right (Right ()) -> Left TimeoutError
  Left (Right response) -> Right response

fetchOffer :: [Word8] -> IO (Either FetchError ByteString)
fetchOffer xid = do
  (receive, awaitBound) <- receiveDhcpOffer
  fmap interpretRace . raceCatch receive $ do
    awaitBound
    sendDhcpDnsRequest xid
    threadDelay 10000000 -- 10s

