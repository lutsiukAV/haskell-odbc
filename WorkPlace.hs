module WorkPlace(
    WorkPlace(WorkPlace),
    workplaceId,
    workplaceNumber,
    workplaceClassId,
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

type WorkPlaceID = Int32
type WorkPlaceNumber = Int32
type WorkPlaceClassID = Int32

data WorkPlace = WorkPlace {
    workplaceId :: Int32,
    workplaceNumber :: Int32,
    workplaceClassId :: Int32
} deriving (Show)

instance Entity WorkPlace where
    create workpl conn = workplace_create (workplaceNumber workpl) (workplaceClassId workpl) conn
    update workpl conn = workplace_update (workplaceId workpl) (workplaceNumber workpl) (workplaceClassId workpl) conn
    delete workpl conn = workplace_delete (workplaceId workpl) conn
    find_all conn = workplace_all conn
    find_id id conn = workplace_id id conn
    display lst = workplace_display lst

workplace_create :: IConnection a => WorkPlaceNumber -> WorkPlaceClassID -> a -> IO Bool
workplace_create number classId conn = do
    withTransaction conn (workplace_create' number classId)
workplace_create' number classId conn = do
    commited <- run conn query [SqlInt32 number, SqlInt32 classId]
    return $ commited == 1
    where
        query = "INSERT INTO workplace (number, class_id) VALUES (?, ?)"

workplace_update :: IConnection a => WorkPlaceID -> WorkPlaceNumber -> WorkPlaceClassID -> a -> IO Bool
workplace_update wpid number classId conn = do
    withTransaction conn (workplace_update' wpid number classId)
workplace_update' wpid number classId conn = do
    commited <- run conn query [SqlInt32 number, SqlInt32 classId, SqlInt32 wpid]
    return $ commited == 1
    where
        query = "UPDATE workplace SET number = ?, class_id = ? WHERE id = ?"

workplace_delete :: IConnection a => WorkPlaceID -> a -> IO Bool
workplace_delete wpid conn = do
    withTransaction conn (workplace_delete' wpid)
workplace_delete' wpid conn = do
    commited <- run conn query [SqlInt32 wpid]
    return $ commited == 1
    where
        query = "DELETE FROM workplace WHERE id = ?"

workplace_all :: IConnection a => a -> IO [WorkPlace]
workplace_all conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = "SELECT * FROM workplace"
        unpuck [SqlInt32 id, SqlInt32 number, SqlInt32 classId] = WorkPlace{workplaceId=id,workplaceNumber=number,workplaceClassId=classId}
        unpuck x = error $ "Unexpected result: " ++ show x

workplace_id :: IConnection a => Int32 -> a -> IO [WorkPlace]
workplace_id id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM workplace WHERE id = %d" id
        unpuck [SqlInt32 id, SqlInt32 number, SqlInt32 classId] = WorkPlace{workplaceId=id,workplaceNumber=number,workplaceClassId=classId}
        unpuck x = error $ "Unexpected result: " ++ show x

workplace_display :: [WorkPlace] -> IO[()]
workplace_display lst = do
    sequence (map workplace_display' lst)
workplace_display' wpl = do
    putStrLn (printf "%d. #%d, class %d" (workplaceId wpl) (workplaceNumber wpl) (workplaceClassId wpl))