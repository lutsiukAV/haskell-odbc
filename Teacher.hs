module Teacher(
    Teacher(Teacher),
    teacherId,
    teacherName,
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
import qualified Data.ByteString.Char8 as BS
import Data.Int
import Text.Printf

type TeacherID = Int32
type TeacherName = String

data Teacher = Teacher {
    teacherId :: Int32,
    teacherName :: String
} deriving (Show)

instance Entity Teacher where
    create teach conn = teacher_create (teacherName teach) conn
    update teach conn = teacher_update (teacherId teach) (teacherName teach) conn
    delete teach conn = teacher_delete (teacherId teach) conn
    find_all conn = teacher_all conn
    find_id id conn = teacher_find_id id conn
    display lst = teacher_display lst

teacher_create :: IConnection a => TeacherName -> a -> IO Bool
teacher_create name conn = do
    withTransaction conn (teacher_create' name)
teacher_create' name conn = do
    commited <- run conn query [SqlString name]
    return $ commited == 1
    where
        query = "INSERT INTO teacher (name) VALUES (?)"

teacher_update :: IConnection a => TeacherID -> TeacherName -> a -> IO Bool
teacher_update tid name conn = do
    withTransaction conn (teacher_update' tid name)
teacher_update' tid name conn = do
    commited <- run conn query [SqlString name, SqlInt32 tid]
    return $ commited == 1
    where
        query = "UPDATE teacher SET name = ? WHERE id = ?"

teacher_delete :: IConnection a => TeacherID -> a -> IO Bool
teacher_delete tid conn = do
    withTransaction conn (teacher_delete' tid)
teacher_delete' tid conn = do
    commited <- run conn query [SqlInt32 tid]
    return $ commited == 1
    where
        query = "DELETE FROM teacher WHERE id = ?"

teacher_all :: IConnection a => a -> IO [Teacher]
teacher_all conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = "SELECT * FROM teacher"
        unpuck [SqlInt32 id, SqlByteString name] = Teacher{teacherId=id,teacherName=(BS.unpack name)}
        unpuck x = error $ "Unexpected result: " ++ show x

teacher_find_id :: IConnection a => Int32 -> a -> IO [Teacher]
teacher_find_id id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM teacher WHERE id = %d" id
        unpuck [SqlInt32 id, SqlByteString name] = Teacher{teacherId=id,teacherName=(BS.unpack name)}
        unpuck x = error $ "Unexpected result: " ++ show x

teacher_display :: [Teacher] -> IO[()]
teacher_display lst = do
    sequence (map teacher_display' lst)
teacher_display' teach = do
    putStrLn ((printf "%d. " (teacherId teach)) ++ (teacherName teach))