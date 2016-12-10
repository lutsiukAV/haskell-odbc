module Schedule(
    Schedule(Schedule),
    scheduleId,
    scheduleDay,
    scheduleNumber,
    scheduleName,
    scheduleTeacherId,
    scheduleTeacherName,
    scheduleClassId,
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

type ScheduleID = Int32
type ScheduleDay = String
type ScheduleNumber = Int32
type ScheduleName = String
type ScheduleTeacherId = Int32
type ScheduleClassId = Int32

data Schedule = Schedule {
    scheduleId :: Int32,
    scheduleDay :: String,
    scheduleNumber :: Int32,
    scheduleName :: String,
    scheduleTeacherId :: Int32,
    scheduleTeacherName :: String,
    scheduleClassId :: Int32
} deriving (Show)

instance Entity Schedule where
    create sched conn = schedule_create (scheduleDay sched) (scheduleNumber sched) (scheduleName sched) (scheduleTeacherId sched) (scheduleClassId sched) conn
    update sched conn = schedule_update (scheduleId sched) (scheduleDay sched) (scheduleNumber sched) (scheduleName sched) (scheduleTeacherId sched) (scheduleClassId sched) conn
    delete sched conn = schedule_delete (scheduleId sched) conn
    find_all conn = schedule_all conn
    find_id id conn = schedule_find_id id conn
    display lst = schedule_display lst

schedule_create :: IConnection a => ScheduleDay -> ScheduleNumber -> ScheduleName -> ScheduleTeacherId -> ScheduleClassId -> a -> IO Bool
schedule_create day num name tid cid conn = do
    withTransaction conn (schedule_create' day num name tid cid )
schedule_create' day num name tid cid conn = do
    commited <- run conn query [SqlString day, SqlInt32 num, SqlString name, SqlInt32 tid, SqlInt32 cid]
    return $ commited == 1
    where
        query = "INSERT INTO schedule (day, number, name, teacher_id, class_id) VALUES (?, ?, ?, ?, ?)"

schedule_update :: IConnection a => ScheduleID -> ScheduleDay -> ScheduleNumber -> ScheduleName -> ScheduleTeacherId -> ScheduleClassId -> a -> IO Bool
schedule_update sid day num name tid cid conn = do
    withTransaction conn (schedule_update' sid day num name tid cid)
schedule_update' sid day num name tid cid conn = do
    commited <- run conn query [SqlString day, SqlInt32 num, SqlString name, SqlInt32 tid, SqlInt32 cid, SqlInt32 sid]
    return $ commited == 1
    where
        query = "UPDATE schedule SET day = ?, number = ?, name = ?, teacher_id = ?, class_id = ? WHERE id = ?"

schedule_delete :: IConnection a => ScheduleID -> a -> IO Bool
schedule_delete sid conn = do
    withTransaction conn (schedule_delete' sid)
schedule_delete' sid conn = do
    commited <- run conn query [SqlInt32 sid]
    return $ commited == 1
    where
        query = "DELETE FROM schedule WHERE id = ?"

schedule_all :: IConnection a => a -> IO [Schedule]
schedule_all conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = "SELECT * FROM schedule_view"
        unpuck [SqlInt32 sid, SqlByteString day, SqlInt32 num, SqlByteString name, SqlInt32 cid, SqlByteString tid] = Schedule{scheduleId=sid,scheduleDay=(BS.unpack day),scheduleNumber=num,scheduleName=(BS.unpack name),scheduleTeacherId=0,scheduleTeacherName=(BS.unpack tid),scheduleClassId=cid}
        unpuck x = error $ "Unexpected result: " ++ show x

schedule_find_id :: IConnection a => Int32 -> a -> IO [Schedule]
schedule_find_id id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM schedule_view WHERE id = %d" id
        unpuck [SqlInt32 sid, SqlByteString day, SqlInt32 num, SqlByteString name, SqlInt32 cid, SqlByteString tid] = Schedule{scheduleId=sid,scheduleDay=(BS.unpack day),scheduleNumber=num,scheduleName=(BS.unpack name),scheduleTeacherId=0,scheduleTeacherName=(BS.unpack tid),scheduleClassId=cid}
        unpuck x = error $ "Unexpected result: " ++ show x

schedule_display :: [Schedule] -> IO[()]
schedule_display lst = do
    sequence (map schedule_display' lst)
schedule_display' sched = do
    putStrLn ((printf "%d. " (scheduleId sched)) ++ (scheduleDay sched) ++ (printf " %d lesson, " (scheduleNumber sched)) ++ (scheduleName sched) ++ (printf " class %d, teacher " (scheduleClassId sched)) ++ (scheduleTeacherName sched))