import Data.Time
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Time.Calendar.WeekDate (toWeekDate)

data State = State
  { stYear  :: Int
  , stMonth :: Int
  }

main :: IO ()
main = do
    putStrLn "Enter year:"
    year <- readLn :: IO Int
    putStrLn "Enter Month (1-12):"
    month <- readLn :: IO Int
    let initialState = State year month
    play
        (InWindow "Calendar" (500, 600) (150, 150))
        cyan
        30
        initialState
        drawState
        handleEvent
        update

drawState :: State -> Picture
drawState (State year month) =
    let monthLength = gregorianMonthLength (toInteger year) month
        firstDay = getFirstDayOfMonth year month
        (_, _, dayOfWeek) = toWeekDate firstDay
    in pictures
        [ drawing monthLength dayOfWeek month
        , nextArrow
        ]

drawing :: Int -> Int -> Int -> Picture
drawing days firstDayOfWeek monthVal =
    pictures (daysOfWeek ++ map (createSquare firstDayOfWeek) (generatePositions days firstDayOfWeek)
              ++ dayNumbers days firstDayOfWeek ++ monthNameText monthVal)

generatePositions :: Int -> Int -> [Point]
generatePositions days firstDayOfWeek =
    [ ((fromIntegral x * squareSize) - gridOffset, fromIntegral y * (-squareSize) - weekLabelOffset)
    | i <- [1 .. days]
    , let x = (i + (firstDayOfWeek - 1)) `mod` 7
    , let y = (i + (firstDayOfWeek - 1)) `div` 7
    ]

squareSize :: Float
squareSize = 50

gridOffset :: Float
gridOffset = squareSize * 3

weekLabelOffset :: Float
weekLabelOffset = 25

labelAdjustment :: Float
labelAdjustment = 20

daysOfWeek :: [Picture]
daysOfWeek =
    [ translate (x - gridOffset - labelAdjustment) weekLabelOffset $ scale 0.2 0.2 $ text label
    | (label, x) <- zip ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"] [0, squareSize .. (6 * squareSize)]
    ]

createSquare :: Int -> Point -> Picture
createSquare _ (x, y) = translate x y $ color black $ rectangleWire squareSize squareSize

intToInteger :: Int -> Integer
intToInteger = fromIntegral

getFirstDayOfMonth :: Int -> Int -> Day
getFirstDayOfMonth year month = fromGregorian (toInteger year) month 1

dayOfWeekToString :: Int -> String
dayOfWeekToString 1 = "Monday"
dayOfWeekToString 2 = "Tuesday"
dayOfWeekToString 3 = "Wednesday"
dayOfWeekToString 4 = "Thursday"
dayOfWeekToString 5 = "Friday"
dayOfWeekToString 6 = "Saturday"
dayOfWeekToString 7 = "Sunday"
dayOfWeekToString _ = "Unknown"

monthNumToName :: Int -> String
monthNumToName 1 = "January"
monthNumToName 2 = "February"
monthNumToName 3 = "March"
monthNumToName 4 = "April"
monthNumToName 5 = "May"
monthNumToName 6 = "June"
monthNumToName 7 = "July"
monthNumToName 8 = "August"
monthNumToName 9 = "September"
monthNumToName 10 = "October"
monthNumToName 11 = "November"
monthNumToName 12 = "December"
monthNumToName _ = "Improper Month"

monthNameText :: Int -> [Picture]
monthNameText monthNum =
    [scale 0.4 0.4 (translate (-425) 200 (text (monthNumToName monthNum)))]

nextMonth :: (Int, Int) -> (Int, Int)
nextMonth (month, year)
    | month >= 12 = (1, year + 1)
    | otherwise   = (month + 1, year)

dayNumbers :: Int -> Int -> [Picture]
dayNumbers days firstDayOfWeek =
    [ translate
        ((fromIntegral x * squareSize) - gridOffset + dayOffsetX)
        (fromIntegral y * (-squareSize) - weekLabelOffset + dayOffsetY)
        (scale 0.2 0.2 $ text (show day))
    | day <- [1 .. days]
    , let x = (day + (firstDayOfWeek - 1)) `mod` 7
    , let y = (day + (firstDayOfWeek - 1)) `div` 7
    ]

dayOffsetX :: Float
dayOffsetX = -6

dayOffsetY :: Float
dayOffsetY = -squareSize / 4

-- A simple arrow to indicate "next month"
-- We'll just draw a triangle pointing to the right in the top-right corner
nextArrow :: Picture
nextArrow = 
    let arrowCoords = [(0,0),(20,10),(0,20)] -- A small triangle
        tx = 150
        ty = 200
    in translate tx ty $ color red $ polygon arrowCoords

-- Handle mouse clicks. If user clicks on the arrow region, go to next month.
handleEvent :: Event -> State -> State
handleEvent (EventKey (MouseButton LeftButton) Up _ (mx, my)) st =
    if isClickOnArrow (mx, my)
       then let (m,y) = (stMonth st, stYear st)
                (m2,y2) = nextMonth (m,y)
            in st { stMonth = m2, stYear = y2 }
       else st
handleEvent _ st = st

-- Check if the click is within the triangle area
isClickOnArrow :: (Float, Float) -> Bool
isClickOnArrow (mx, my) =
    let tx = 150
        ty = 200
        -- The arrow is roughly a triangle around (tx, ty)
        -- bounding box of the arrow triangle: x in [tx, tx+20], y in [ty, ty+20]
    in mx >= tx && mx <= tx+20 && my >= ty && my <= ty+20

update :: Float -> State -> State
update _ st = st
