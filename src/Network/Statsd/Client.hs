{-# LANGUAGE ViewPatterns #-}
module Network.Statsd.Client
        ( -- * Types
          StatsD
        , Metric
        , Name
        , Socket
        , HostName
        , PortNumber(..)
        -- Connecting to servers
        , Network.Statsd.Client.connect, connectOn, Network.Statsd.Client.close, withStatsd
        -- Building metrics
        , guage, increment, decrement, histogram, meter, heartbeat
        -- * Sending metrics
        , sendMetric, sendMetrics
        -- * Low level operations
        , socketOf
        ) where

import qualified Network.Socket as N
import Network.Socket (PortNumber, HostName, Socket, send, addrsocketType, AF_INET, getAddrInfo, defaultHints)
import Data.Maybe (listToMaybe)
import Control.Monad (void)
import Control.Exception (bracket)

type Name   = String
data StatsD = StatsD HostName PortNumber Socket

-- | Obtain the socket used for communicating to the statsd server.
socketOf :: StatsD -> Socket
socketOf (StatsD _ _ s) = s

-- | Obtain a socket for sending to a statsd server
-- at the specified hostname.
connect :: HostName -> IO StatsD
connect h = connectOn h 8125

-- | Obtain a socket for sending to a statsd server
-- at the specified hostname and port.
connectOn :: HostName -> PortNumber -> IO StatsD
connectOn hst port = do
    sAddr <- resolve hst port
    sock <- socket AF_INET Datagram defaultProtocol
    N.connect sock sAddr
    return (StatsD hst port sock)
 where
  resolve :: HostName -> PortNumber -> IO SockAddr
  resolve h p = do
      ai <- getAddrInfo (Just $ defaultHints { addrFamily = AF_INET, addrSocketType = Datagram } ) (Just h) (Just (show p))
      return (maybe (error $ "Could not resolve host " ++ h) addrAddress (listToMaybe ai))

-- | Close a socket used for StatsD
close :: StatsD -> IO ()
close = N.close . socketOf

-- Open a socket to a StatsD server for the duration of the IO action.
withStatsd :: HostName -> PortNumber -> (StatsD -> IO a) -> IO a
withStatsd h p io = bracket (connectOn h p) close io

showRailed :: (Bounded i, Show i, Integral i) => i -> String
showRailed i
  | fromIntegral minI > high && high > fromIntegral i = show high
  | fromIntegral maxI < low  &&  low < fromIntegral i = show low
  | otherwise                                         = show i
  where
      low  = negate 2^64
      high = 2^64 - 1
      minI = minBound `asTypeOf` i
      maxI = maxBound `asTypeOf` i

guage     :: (Show int, Bounded int, Integral int) => Name -> int -> Metric
guage m i = Metric (m ++ ":" ++ showRailed i ++ "|g")

increment :: (Show int, Bounded int, Integral int) => Name -> int -> Maybe Double -> Metric
increment  m i sampRate =
    Metric (m ++ ":" ++ showRailed i ++ "|c" ++ toStr sampRate)
  where
    toStr Nothing  = ""
    toStr (Just d) = "|@" ++ show d

decrement :: (Show int, Bounded int, Integral int) => Name -> int -> Maybe Double -> Metric
decrement  m i sampRate =
    Metric (m ++ ":" ++ showRailed (-i) ++ "|c" ++ toStr sampRate)
  where
    toStr Nothing  = ""
    toStr (Just d) = "|@" ++ show d

-- | A time (in ms)
timing    :: (Show int, Bounded int, Integral int) => Name -> int -> Metric
timing     m i = Metric (m ++ ":" ++ showRailed i ++ "|ms")

histogram :: (Show int, Bounded int, Integral int) => Name -> int -> Metric
histogram  m i = Metric (m ++ ":" ++ showRailed i ++ "|h")

meter     :: (Show int, Bounded int, Integral int) => Name -> int -> Metric
meter      m i = Metric (m ++ ":" ++ showRailed i ++ "|m")

heartbeat :: Name -> Metric
heartbeat n = Metric ("heartbeat." ++ n ++":1|c")

-- | Send a single metric
sendMetric :: StatsD -> Metric -> IO ()
sendMetric s m = sendMetrics s [m]

-- | Send many metrics
sendMetrics :: StatsD -> [Metric] -> IO ()
sendMetrics (socketOf -> s) = void . send s . unlines . map metricString

newtype Metric = Metric { metricString :: String } deriving (Eq, Ord, Show)
