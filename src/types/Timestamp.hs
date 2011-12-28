
module Timestamp (Timestamp, getStart, getFinish, getDuration, tsHuman, tsJSON) where

-- | 'Timestamp' defines a tuple of start and finish int's
type Timestamp = (Int, Int)

-- | 'getStart' gets the start part of a timestamp
getStart :: Timestamp -> Int
getStart (x, _) = x

-- | 'getFinish' gets the finish part of a timestamp
getFinish :: Timestamp -> Int
getFinish (_, y) = y

-- | 'getDuration' calculates the difference between start and finish values
getDuration :: Timestamp -> Int
getDuration (x, y) = y-x

-- | 'tsHuman' returns a string for humans representing the values of the timestamp
tsHuman :: Timestamp -> String
tsHuman (s, f) = "Timestamp " ++ show s ++ " -> " ++ show f ++ " (" ++ show (f-s) ++ ")"

-- | 'tsJSON' returns a string for JSON representing the values of the timestamp
tsJSON :: Timestamp -> String
tsJSON (s, f) = "\"timestamp\": {\"start\": \"" ++ show s ++ "\", \"finish\": \"" ++ show f ++ "\"}"