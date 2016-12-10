module ComputerClass(
    ComputerClass(ComputerClass),
    cclassId,
    cclassName,
    cclassPlaces,
    create,
    update,
    delete,
    find_all,
    find_id,
    display
    ) where

import Entity
import Database.HDBC.ODBC
import Database.HDBC
import Data.Int
import Text.Printf

type ComputerClassID = Int32
type ComputerClassName = Int32
type ComputerClassPlaces = Int32

data ComputerClass = ComputerClass {
    cclassId :: Int32,
    cclassName :: Int32,
    cclassPlaces :: Int32
} deriving (Show)

instance Entity ComputerClass where
    create ccls conn = cclass_create (cclassName ccls) (cclassPlaces ccls) conn
    update ccls conn = cclass_update (cclassId ccls) (cclassName ccls) (cclassPlaces ccls) conn
    delete ccls conn = cclass_delete (cclassId ccls) conn
    find_all conn = cclass_all conn
    find_id id conn = cclass_find_id id conn
    display lst = cclass_display lst

cclass_create :: IConnection a => ComputerClassName -> ComputerClassPlaces -> a -> IO Bool
cclass_create name places conn = do
    withTransaction conn (cclass_create' name places)
cclass_create' name places conn = do
    commited <- run conn query [SqlInt32 name, SqlInt32 places]
    return $ commited == 1
    where
        query = "INSERT INTO compclass (name, places) VALUES (?, ?)"

cclass_update :: IConnection a => ComputerClassID -> ComputerClassName -> ComputerClassPlaces -> a -> IO Bool
cclass_update ccid name places conn = do
    withTransaction conn (cclass_update' ccid name places)
cclass_update' ccid name places conn = do
    commited <- run conn query [SqlInt32 name, SqlInt32 places, SqlInt32 ccid]
    return $ commited == 1
    where
        query = "UPDATE compclass SET name = ?, places = ? WHERE id = ?"

cclass_delete :: IConnection a => ComputerClassID -> a -> IO Bool
cclass_delete ccid conn = do
    withTransaction conn (cclass_delete' ccid)
cclass_delete' ccid conn = do
    commited <- run conn query [SqlInt32 ccid]
    return $ commited == 1
    where
        query = "DELETE FROM compclass WHERE id = ?"

cclass_all :: IConnection a => a -> IO [ComputerClass]
cclass_all conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = "SELECT * FROM compclass"
        unpuck [SqlInt32 id, SqlInt32 name, SqlInt32 places] = ComputerClass{cclassId=id,cclassName=name,cclassPlaces=places}
        unpuck x = error $ "Unexpected result: " ++ show x

cclass_find_id :: IConnection a => Int32 -> a -> IO [ComputerClass]
cclass_find_id id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM compclass WHERE id = %d" id
        unpuck [SqlInt32 id, SqlInt32 name, SqlInt32 places] = ComputerClass{cclassId=id,cclassName=name,cclassPlaces=places}
        unpuck x = error $ "Unexpected result: " ++ show x

cclass_display :: [ComputerClass] -> IO[()]
cclass_display lst = do
    sequence (map cclass_display' lst)
cclass_display' ccls = do
    putStrLn (printf "%d. #%d, %d places" (cclassId ccls) (cclassName ccls) (cclassPlaces ccls))