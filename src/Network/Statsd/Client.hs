{-# LANGUAGE ViewPatterns #-}
module Network.Statsd.Client 
        ( -- * Types
          StatsD
        , Metric
        , Name
        -- Connecting to servers
        , clientTo, clientToOn
        -- Building metrics
        , guage, increment, decrement, histogram, meter, heartbeat
        -- * Sending metrics
        , sendMetric, sendMetrics
        -- * Low level operations
        , socketOf
        ) where
import Network.Socket
import Data.Maybe (listToMaybe)
import Control.Monad (void)

type Name   = String
data StatsD = StatsD HostName PortNumber Socket

socketOf :: StatsD -> Socket
socketOf (StatsD _ _ s) = s

clientTo :: HostName -> IO StatsD
clientTo h = clientToOn h 8125

clientToOn :: HostName -> PortNumber -> IO StatsD
clientToOn hst port = do
    sAddr <- resolve hst port
    sock <- socket AF_INET Datagram defaultProtocol
    connect sock sAddr
    return (StatsD hst port sock)
 where
  resolve :: HostName -> PortNumber -> IO SockAddr
  resolve h p = do
      ai <- getAddrInfo (Just $ defaultHints { addrFamily = AF_INET, addrSocketType = Datagram } ) (Just h) (Just (show p))
      return (maybe (error $ "Could not resolve host " ++ h) addrAddress (listToMaybe ai))

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

timing    :: (Show int, Bounded int, Integral int) => Name -> int -> Metric
timing     m i = Metric (m ++ ":" ++ showRailed i ++ "|ms")

histogram :: (Show int, Bounded int, Integral int) => Name -> int -> Metric
histogram  m i = Metric (m ++ ":" ++ showRailed i ++ "|h")


meter     :: (Show int, Bounded int, Integral int) => Name -> int -> Metric
meter      m i = Metric (m ++ ":" ++ showRailed i ++ "|m")

heartbeat :: Name -> Metric
heartbeat n = Metric ("heartbeat." ++ n ++":1|c")

-- Send a single metric
sendMetric :: StatsD -> Metric -> IO ()
sendMetric s m = sendMetrics s [m]

sendMetrics :: StatsD -> [Metric] -> IO ()
sendMetrics (socketOf -> s) = void . send s . unlines . map show

newtype Metric = Metric { metricString :: String }

instance Show Metric where
    show = metricString
