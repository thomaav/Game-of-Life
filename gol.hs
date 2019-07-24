import UI.NCurses

import Data.Set as Set

import Control.Monad (forM_)

data Point =
  Point Integer Integer
  deriving (Show, Ord, Eq)

data CellState
  = Dead
  | Alive
  deriving (Show, Ord, Eq)

data Cell =
  Cell Point CellState
  deriving (Show, Ord)

instance Eq Cell where
  (Cell (Point x1 y1) _) == (Cell (Point x2 y2) _) = x1 == x2 && y1 == y2

type CA = Set Cell

cellToString :: CellState -> String
cellToString Alive = "#"
cellToString Dead = " "

silver :: Curses ColorID
silver = newColorID ColorBlack (Color 7) 1

gray :: Curses ColorID
gray = newColorID ColorBlack (Color 15) 2

-- TODO: Learn lenses for accessing?
drawCell :: Window -> Cell -> Curses ()
drawCell w (Cell (Point x y) state) = do
  colorID <-
    if even (x + y)
      then gray
      else silver
  updateWindow w $ do setColor colorID
  drawStringAt w x y $ cellToString state

drawAutomata :: Window -> CA -> Curses ()
drawAutomata w ca = forM_ ca $ \c -> do drawCell w c

drawStringAt :: Window -> Integer -> Integer -> String -> Curses ()
drawStringAt w x y str =
  updateWindow w $ do
    moveCursor y x
    drawString str

isMouseClick :: Event -> Bool
isMouseClick (EventMouse _ _) = True
isMouseClick _ = False

mouseToggle :: CA -> Event -> CA
mouseToggle ca (EventMouse _ (MouseState coords _ _ _ _)) =
  case coords of
    (x, y, z) -> insert (toggleCell c) . Set.filter (\c' -> c' /= c) $ ca
      where c = getCell ca (Point x y)

getCell :: CA -> Point -> Cell
getCell ca p = head . toList . Set.filter (\c -> c == Cell p Alive) $ ca

toggleCell :: Cell -> Cell
toggleCell (Cell p state) = Cell p nextState
  where
    nextState
      | state == Alive = Dead
      | otherwise = Alive

getNeighbors :: CA -> Cell -> Set Cell
getNeighbors ca (Cell (Point x y) _) =
  fromList . Prelude.filter (\c -> member c neighbors) . toList $ ca
  where
    neighbors =
      fromList
        [ Cell (Point (x + dx) (y + dy)) Alive
        | dx <- [-1 .. 1]
        , dy <- [-1 .. 1]
        , not (dx == 0 && dy == 0)
        ]

isAlive :: Cell -> Bool
isAlive (Cell _ state) = state == Alive

livingNeighbors :: CA -> Cell -> Int
livingNeighbors ca c =
  length . Set.filter (\c' -> isAlive c') $ getNeighbors ca c

lives :: CA -> Cell -> Bool
lives ca c
  | isAlive c = livingNeighborCount `elem` [2, 3]
  | otherwise = livingNeighborCount == 3
  where
    livingNeighborCount = livingNeighbors ca c

stepCell :: CA -> Cell -> Cell
stepCell ca c@(Cell p _)
  | lives ca c = Cell p Alive
  | otherwise = Cell p Dead

stepCA :: CA -> CA
stepCA ca = Set.map (\c -> stepCell ca c) ca

width :: Integer
width = 50

height :: Integer
height = 20

golCA :: CA
golCA = fromList [Cell (Point x y) Dead | x <- [0 .. width], y <- [0 .. height]]

rungol :: Window -> CA -> Curses ()
rungol w ca = loop golCA
  where
    loop ca = do
      drawAutomata w ca
      render
      ev <- getEvent w Nothing
      case ev of
        Nothing -> loop . stepCA $ ca
        Just ev'
          | shouldQuit ev' -> return ()
          | isMouseClick ev' -> loop $ mouseToggle ca ev'
          | ev' == EventCharacter 'n' -> loop . stepCA $ ca
          | otherwise -> loop ca

shouldQuit :: Event -> Bool
shouldQuit ev = ev == EventCharacter 'q' || ev == EventCharacter 'Q'

-- TODO: Monad transformer for IO inside Curses ()?
main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    rungol w ca
  where
    ca = golCA
