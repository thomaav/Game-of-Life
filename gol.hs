{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Lens.TH
import Control.Monad (forM_)

import Data.Set as Set

import UI.NCurses

data Point =
  Point
    { _x :: Integer
    , _y :: Integer
    }
  deriving (Show, Ord, Eq)

$(makeLenses ''Point)

data CellState
  = Dead
  | Alive
  deriving (Show, Ord, Eq)

data Cell =
  Cell Point CellState
  deriving (Show, Ord)

instance Eq Cell where
  (Cell p1 _) == (Cell p2 _) = p1 ^. x == p2 ^. x && p1 ^. y == p2 ^. y

type CA = Set Cell

cellToString :: CellState -> String
cellToString Alive = "#"
cellToString Dead = " "

silver :: Curses ColorID
silver = newColorID ColorBlack (Color 7) 1

gray :: Curses ColorID
gray = newColorID ColorBlack (Color 15) 2

drawCell :: Window -> Cell -> Curses ()
drawCell w (Cell p state) = do
  colorID <-
    if even ((p ^. x) + (p ^. y))
      then gray
      else silver
  updateWindow w $ do setColor colorID
  drawStringAt w (p ^. x) (p ^. y) $ cellToString state

drawAutomata :: Window -> CA -> Curses ()
drawAutomata w ca = forM_ ca $ \c -> do drawCell w c

renderAutomata :: Window -> CA -> Curses ()
renderAutomata w ca = do
  drawAutomata w ca
  render

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
    (x, y, z) -> toggleCACell ca (Point x y)

getCell :: CA -> Point -> Cell
getCell ca p = head . toList . Set.filter (\c -> c == Cell p Alive) $ ca

toggleCACell :: CA -> Point -> CA
toggleCACell ca p = insert (toggleCell c) . Set.filter (\c' -> c' /= c) $ ca
  where
    c = getCell ca p

toggleCell :: Cell -> Cell
toggleCell (Cell p state) = Cell p nextState
  where
    nextState
      | state == Alive = Dead
      | otherwise = Alive

getNeighbors :: CA -> Cell -> Set Cell
getNeighbors ca (Cell p _) =
  fromList . Prelude.filter (\c -> member c neighbors) . toList $ ca
  where
    neighbors =
      fromList
        [ Cell (p & x %~ (+ dx) & y %~ (+ dy)) Alive
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

runDelay :: Maybe Integer
runDelay = Just 50

golCA :: CA
golCA =
  toggleCACell' (Point 25 9) .
  toggleCACell' (Point 24 10) .
  toggleCACell' (Point 25 10) . toggleCACell' (Point 25 11) $
  initialCA
  where
    initialCA =
      fromList [Cell (Point x y) Dead | x <- [0 .. width], y <- [0 .. height]]
    toggleCACell' = flip toggleCACell

rungol :: Window -> CA -> Curses ()
rungol w ca = go golCA Nothing
  where
    go ca timeout = do
      renderAutomata w ca
      ev <- getEvent w timeout
      case ev of
        Nothing -> go (stepCA ca) timeout
        Just ev'
          | shouldQuit ev' -> return ()
          | isMouseClick ev' -> go (mouseToggle ca ev') timeout
          | ev' == EventCharacter 'n' -> go ca runDelay
          | ev' == EventCharacter 'p' -> go ca Nothing
          | otherwise -> go ca timeout

shouldQuit :: Event -> Bool
shouldQuit ev = ev == EventCharacter 'q' || ev == EventCharacter 'Q'

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    rungol w ca
  where
    ca = golCA
