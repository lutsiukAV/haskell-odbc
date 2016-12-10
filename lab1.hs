import Teacher
import ComputerClass
import WorkPlace
import Schedule
import Placement
import Database.HDBC.ODBC
import Database.HDBC
import System.IO
import System.Exit
import Control.Monad
import Data.Int
import Text.Printf

main = forever (printMenu >> readChoice >>= menuAction)

printMenu = do
    putStrLn "\nTeacher section:"
    putStrLn " 1) insert teacher"
    putStrLn " 2) update teacher"
    putStrLn " 3) delete teacher"
    putStrLn " 4) print all teachers"
    putStrLn " 5) print teacher by id"
    putStrLn "\nClass section:"
    putStrLn " 6) insert class"
    putStrLn " 7) update class"
    putStrLn " 8) delete class"
    putStrLn " 9) print all classes"
    putStrLn "10) print class by id"
    putStrLn "\nPlace section:"
    putStrLn "11) insert place"
    putStrLn "12) update place"
    putStrLn "13) delete place"
    putStrLn "14) print all places"
    putStrLn "15) print place by id"
    putStrLn "\nSchedule section:"
    putStrLn "16) insert lesson"
    putStrLn "17) update lesson"
    putStrLn "18) delete lesson"
    putStrLn "19) print full schedule"
    putStrLn "20) print lesson by id"
    putStrLn "\nPlacement section:"
    putStrLn "21) insert placement"
    putStrLn "22) print full placement\n"
    putStrLn "23) exit"
    putStr "\nYour choice: " 
    hFlush stdout

readChoice = {-hSetBuffering stdin NoBuffering >> hSetEcho stdin True >>-} getLine

readInt32 = do
    s <- getLine
    return (read s :: Int32)

-- Menu actions

menuAction "1" = do
    putStrLn "Teacher name:"
    nm <- getLine
    conn <- connectODBC "DSN=MySQL_Lab1"
    create Teacher{teacherId=1,teacherName=nm} conn
    putStrLn "-- ok"

menuAction "2" = do
    putStrLn "Teacher id:"
    tid <- readInt32
    putStrLn "Teacher name:"
    nm <- getLine
    conn <- connectODBC "DSN=MySQL_Lab1"
    update Teacher{teacherId=tid,teacherName=nm} conn
    putStrLn "-- ok"

menuAction "3" = do
    putStrLn "Teacher id:"
    tid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    delete Teacher{teacherId=tid,teacherName=""} conn
    putStrLn "-- ok"

menuAction "4" = do
    conn <- connectODBC "DSN=MySQL_Lab1"
    lst <- find_all conn :: IO[Teacher]
    display lst
    putStrLn "-- end"

menuAction "5" = do
    putStrLn "Teacher id:"
    tid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    lst <- find_id tid conn :: IO[Teacher]
    display lst
    putStrLn "-- end"

menuAction "6" = do
    putStrLn "Class number:"
    nm <- readInt32
    putStrLn "Places quantity:"
    pl <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    create ComputerClass{cclassId=1,cclassName=nm,cclassPlaces=pl} conn
    putStrLn "-- ok"

menuAction "7" = do
    putStrLn "Class id:"
    ccid <- readInt32
    putStrLn "Class number:"
    nm <- readInt32
    putStrLn "Places quantity:"
    pl <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    update ComputerClass{cclassId=ccid,cclassName=nm,cclassPlaces=pl} conn
    putStrLn "-- ok"

menuAction "8" = do
    putStrLn "Class id:"
    ccid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1L"
    delete ComputerClass{cclassId=ccid,cclassName=0,cclassPlaces=0} conn
    putStrLn "-- ok"

menuAction "9" = do
    conn <- connectODBC "DSN=MySQL_Lab1"
    lst <- find_all conn :: IO[ComputerClass]
    display lst
    putStrLn "-- end"

menuAction "10" = do
    putStrLn "Class id:"
    ccid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    lst <- find_id ccid conn :: IO[ComputerClass]
    display lst
    putStrLn "-- end"

menuAction "11" = do
    putStrLn "Place number:"
    nm <- readInt32
    putStrLn "Class ID:"
    ccid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1L"
    create WorkPlace{workplaceId=1,workplaceNumber=nm,workplaceClassId=ccid} conn
    putStrLn "-- ok"

menuAction "12" = do
    putStrLn "Place id:"
    wpid <- readInt32
    putStrLn "Place number:"
    nm <- readInt32
    putStrLn "Class ID:"
    ccid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    update WorkPlace{workplaceId=wpid,workplaceNumber=nm,workplaceClassId=ccid} conn
    putStrLn "-- ok"

menuAction "13" = do
    putStrLn "Place id:"
    wpid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    delete WorkPlace{workplaceId=wpid,workplaceNumber=0,workplaceClassId=0} conn
    putStrLn "-- ok"

menuAction "14" = do
    conn <- connectODBC "DSN=MySQL_Lab1"
    lst <- find_all conn :: IO[WorkPlace]
    display lst
    putStrLn "-- end"

menuAction "15" = do
    putStrLn "Place id:"
    wpid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    lst <- find_id wpid conn :: IO[WorkPlace]
    display lst
    putStrLn "-- end"

menuAction "16" = do
    putStrLn "Day (2 letters, e.g. MO, TU, WD, TH, FR, SA, SU):"
    day <- getLine
    putStrLn "Lesson number:"
    num <- readInt32
    putStrLn "Lesson name:"
    name <- getLine
    putStrLn "Teacher ID:"
    tid <- readInt32
    putStrLn "Class ID:"
    ccid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    create Schedule{scheduleId=1,scheduleDay=day,scheduleNumber=num,scheduleName=name,scheduleTeacherId=tid,scheduleTeacherName="",scheduleClassId=ccid} conn
    putStrLn "-- ok"

menuAction "17" = do
    putStrLn "Lesson id:"
    sid <- readInt32
    putStrLn "Day (2 letters, e.g. MO, TU, WD, TH, FR, SA, SU):"
    day <- getLine
    putStrLn "Lesson number:"
    num <- readInt32
    putStrLn "Lesson name:"
    name <- getLine
    putStrLn "Teacher ID:"
    tid <- readInt32
    putStrLn "Class ID:"
    ccid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    update Schedule{scheduleId=sid,scheduleDay=day,scheduleNumber=num,scheduleName=name,scheduleTeacherId=tid,scheduleTeacherName="",scheduleClassId=ccid} conn
    putStrLn "-- ok"

menuAction "18" = do
    putStrLn "Lesson id:"
    sid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    delete Schedule{scheduleId=sid,scheduleDay="00",scheduleNumber=0,scheduleName="",scheduleTeacherId=0,scheduleTeacherName="",scheduleClassId=0} conn
    putStrLn "-- ok"

menuAction "19" = do
    conn <- connectODBC "DSN=MySQL_Lab1"
    lst <- find_all conn :: IO[Schedule]
    display lst
    putStrLn "-- end"

menuAction "20" = do
    putStrLn "Lesson id:"
    sid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    lst <- find_id sid conn :: IO[Schedule]
    display lst
    putStrLn "-- end"

menuAction "21" = do
    putStrLn "Lesson ID:"
    sid <- readInt32
    putStrLn "Place ID:"
    wpid <- readInt32
    conn <- connectODBC "DSN=MySQL_Lab1"
    create Placement{placementLessonId=sid,placementPlaceId=wpid} conn
    putStrLn "-- ok"

menuAction "22" = do
    conn <- connectODBC "DSN=MySQL_Lab1"
    lst <- find_all conn :: IO[Placement]
    display lst
    putStrLn "-- end"

menuAction "23" = exitSuccess

menuAction _ = hPutStrLn stderr "\nInvalid choice."