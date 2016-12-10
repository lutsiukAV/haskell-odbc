module Placement(
    Placement(Placement),
    placementLessonId,
    placementPlaceId,
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

type PlacementLessonID = Int32
type PlacementPlaceID = Int32

data Placement = Placement {
    placementLessonId :: Int32,
    placementPlaceId :: Int32
} deriving (Show)

instance Entity Placement where
    create plcm conn = placement_create (placementLessonId plcm) (placementPlaceId plcm) conn
    find_all conn = placement_all conn
    display lst = placement_display lst

placement_create :: IConnection a => PlacementLessonID -> PlacementPlaceID -> a -> IO Bool
placement_create lid pid conn = do
    withTransaction conn (placement_create' lid pid)
placement_create' lid pid conn = do
    commited <- run conn query [SqlInt32 lid, SqlInt32 pid]
    return $ commited == 1
    where
        query = "INSERT INTO placement (lecture_id, place_id) VALUES (?, ?)"

placement_all :: IConnection a => a -> IO [Placement]
placement_all conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = "SELECT * FROM placement"
        unpuck [SqlInt32 lid, SqlInt32 pid] = Placement{placementLessonId=lid,placementPlaceId=pid}
        unpuck x = error $ "Unexpected result: " ++ show x

placement_display :: [Placement] -> IO[()]
placement_display lst = do
    sequence (map placement_display' lst)
placement_display' plcm = do
    putStrLn (printf "lecture %d - place %d is busy" (placementLessonId plcm) (placementPlaceId plcm))