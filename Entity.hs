module Entity(
    Entity, create, update, delete, find_all, find_id, display
    ) where

import Database.HDBC.ODBC
import Database.HDBC
import Data.Int

class Entity entity where
    create :: IConnection a => entity -> a -> IO Bool
    update :: IConnection a => entity -> a -> IO Bool
    delete :: IConnection a => entity -> a -> IO Bool
    find_all :: IConnection a => a -> IO [entity]
    find_id :: IConnection a => Int32 -> a -> IO [entity]
    display :: [entity] -> IO[()]
