module CVG.Database (connect, createTables, insert, findFingerprints) where

import CVG.Types.Fingerprint
import CVG.Types.Hash
import CVG.Types.Timestamp
import CVG.Types.Backend

import Database.SQLite
import Maybe

type SQLResult = Either String [[Row Value]]

-- | sql statment to create the table
databaseCreateSQL :: String
databaseCreateSQL = unlines [ "create table if not exists fingerprints (",
    "id integer primary key autoincrement, ",
    "host text not null, ",
    "port integer not null, ",
    "start integer not null, ",
    "end integer not null, ",
    "hash text not null,",
    "created_at integer not null)"]

-- | sql statment to insert a new row
databaseInsertSQL:: String
databaseInsertSQL = unlines ["insert into fingerprints",
    "(host, port, start, end, hash, created_at) values",
    "(:host, :port, :start, :end, :hash, strftime('%s','now'))"]

-- | sql statment to select row by host and port
databaseSelectByHostAndPortSQL :: String
databaseSelectByHostAndPortSQL = unlines ["select * from fingerprints",
    " where host == :host and port == :port"]

databaseCheckIfFingerprintsExsists :: String
databaseCheckIfFingerprintsExsists = unlines ["select name from sqlite_master",
    " where type = \"table\" and name = \"fingerprints\""]

-- | establish connection to the database defined in DATABASE_NAME
connect :: String -> IO SQLiteHandle
connect databaseName = do
    db <- openConnection databaseName
    createTables db
    return db

-- | create fingerprints table
createTables :: SQLiteHandle -> IO (Maybe String)
createTables db = execStatement_ db databaseCreateSQL

-- | insert a new row
insert :: SQLiteHandle -> Request -> Response -> IO (Maybe String)
insert db (Request (Host host port) _) (Response [((start, end), hash)]) = execParamStatement_ db databaseInsertSQL [(":host", Text host),
                 (":port" , Int  $ fromIntegral port),
                 (":start", Int  $ fromIntegral start),
                 (":end",  Int  $ fromIntegral end),
                 (":hash", Text hash)]


-- | returns all fingerpints to a given host and port
findFingerprints :: SQLiteHandle -> Request -> IO Response
findFingerprints db (Request (Host host port) _) = do
    res :: SQLResult <- execParamStatement db databaseSelectByHostAndPortSQL 
                [(":host", Text host),(":port",Int $ fromIntegral port)]
    case res of
        Left s -> return (Response [])
        Right rows -> return $ rows2fingerprints $ head rows

-- | gets the value for specified columnname 
getValue :: Row Value -> String -> Value
getValue [] _ = Null
getValue ((column, value):row) name
    | column == name = value
    | otherwise = getValue row name

-- | translate the sqlite rows into the fingerprints
rows2fingerprints :: [Row Value] -> Response
rows2fingerprints [] = Response []
rows2fingerprints (row:rows) =
    Response (((fromIntegral start, fromIntegral end), hash):rest)
    where 
        (Int start) = getValue row "start"
        (Int end) = getValue row "end"
        (Text hash) = getValue row "hash"
        (Response rest) = rows2fingerprints rows

emptyResponse :: Response -> Bool
emptyResponse (Response []) = True
emptyResponse _ = False

dbCache :: SQLiteHandle -> (Request -> IO Response) -> Request -> IO Response
dbCache db scan req = do
        fps <- findFingerprints db req
        if emptyResponse fps
          then do
             resp <- scan req
             insert db req resp
             return resp
          else
             return fps
