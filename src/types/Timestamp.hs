
-- | 'Timestamp' defines a tuple of start and finish ints
type Timestamp = (Int, Int)

-- | 'getStart' gets the start part of a timestamp
getStart :: Timestamp -> Int
getStart (x, _) = x

-- | 'getFinish' gets the finish part of a timestamp
getFinish :: Timestamp -> Int
getFinish (_, y) = y

-- | 'timestampHuman' returns a string for humans representing the values of the timestamp
timestampHuman :: Timestamp -> String
timestampHuman (s, f) = "Timestamp Start: " ++ show s ++ " Finish: " ++ show f

-- | 'timestampJSON' returns a string for JSON representing the values of the timestamp
timestampJSON :: Timestamp -> String
timestampJSON (s, f) = "\"timestamp\": {\"start\": \"" ++ show s ++ "\", \"finish\": \"" ++ show f ++ "\"}"
