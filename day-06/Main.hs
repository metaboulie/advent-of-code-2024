{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (guard)
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Sequence (Seq (..), ViewL (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Debug.Trace

data Direction = North | East | South | West
    deriving (Show, Eq, Ord)

type Position = (Int, Int)
type Grid = Set.Set Position

data State = State
    { position :: Position
    , direction :: Direction
    , visited :: Set.Set Position
    , gridSize :: (Int, Int)
    , obstacles :: Grid
    }
    deriving (Show)

turnRight :: Direction -> Direction
turnRight = \case
    North -> East
    East -> South
    South -> West
    West -> North

nextPosition :: Position -> Direction -> Position
nextPosition (x, y) = \case
    North -> (x, y - 1)
    East -> (x + 1, y)
    South -> (x, y + 1)
    West -> (x - 1, y)

inBounds :: (Int, Int) -> Position -> Bool
inBounds (maxX, maxY) (x, y) =
    x >= 0 && x < maxX && y >= 0 && y < maxY

step :: State -> Maybe State
step state@State{..} = do
    let next = nextPosition position direction
    if not (inBounds gridSize next)
        then Nothing
        else
            if Set.member next obstacles
                then Just $ state{direction = turnRight direction}
                else
                    Just $
                        state
                            { position = next
                            , visited = Set.insert next visited
                            }

simulate :: State -> Int
simulate initial = Set.size $ go initial
  where
    go state = case step state of
        Nothing -> visited state
        Just newState -> go newState

parseGrid :: String -> Maybe State
parseGrid input = do
    let lines' = lines input
    guard (not $ null lines')
    let height = length lines'
    firstLine <- listToMaybe lines'
    let width = length firstLine

    (startX, startY, startDir) <-
        listToMaybe
            [ (x, y, dir)
            | (y, row) <- zip [0 ..] lines'
            , (x, c) <- zip [0 ..] row
            , dir <- case c of
                '^' -> [North]
                'v' -> [South]
                '<' -> [West]
                '>' -> [East]
                _ -> []
            ]

    let startPos = (startX, startY)
        obstacles =
            Set.fromList
                [ (x, y)
                | (y, row) <- zip [0 ..] lines'
                , (x, c) <- zip [0 ..] row
                , c == '#'
                ]

    pure
        State
            { position = startPos
            , direction = startDir
            , visited = Set.singleton startPos
            , gridSize = (width, height)
            , obstacles = obstacles
            }

data TurnPoint = TurnPoint
    { turnPos :: Position
    , turnDir :: Direction
    }
    deriving (Show, Eq, Ord)

data PathState = PathState
    { currentState :: State
    , turnPoints :: Seq TurnPoint
    , loopPositions :: Set.Set Position
    }

isValidLoop :: TurnPoint -> TurnPoint -> TurnPoint -> Position -> Bool
isValidLoop p1 p2 p3 pos =
    let (x1, y1) = turnPos p1
        (x2, y2) = turnPos p2
        (x3, y3) = turnPos p3
        (x4, y4) = pos
     in -- Check if points form a rectangle
        ((x1 - x2) == (x4 - x3) && (y1 - y2) == (y4 - y3))

stepWithTurns :: PathState -> Maybe PathState
stepWithTurns PathState{..} = do
    let State{..} = currentState
        next = nextPosition position direction

    if not (inBounds gridSize next)
        then Nothing
        else
            if Set.member next obstacles
                then
                    let newTurn = TurnPoint position direction
                        newTurnPoints =
                            if Seq.length turnPoints >= 3
                                then Seq.insertAt 3 newTurn (Seq.drop 1 turnPoints)
                                else Seq.insertAt 3 newTurn turnPoints
                     in Just $
                            PathState
                                { currentState = currentState{direction = turnRight direction}
                                , turnPoints = newTurnPoints
                                , loopPositions = loopPositions
                                }
                else
                    let newLoopPositions =
                            if Seq.length turnPoints == 3
                                then Set.union loopPositions (checkLoopPosition turnPoints position next)
                                else loopPositions
                     in Just $
                            PathState
                                { currentState =
                                    currentState
                                        { position = next
                                        , visited = Set.insert next visited
                                        }
                                , turnPoints = turnPoints
                                , loopPositions = newLoopPositions
                                }

checkLoopPosition :: Seq TurnPoint -> Position -> Position -> Set.Set Position
checkLoopPosition turns currentPos next =
    if Seq.length turns < 3
        then Set.empty
        else
            let p1 = Seq.index turns 0
                p2 = Seq.index turns 1
                p3 = Seq.index turns 2
             in if isValidLoop p1 p2 p3 currentPos
                    then Set.singleton next
                    else Set.empty

-- Find all possible loop positions
findLoopPositions :: State -> Int
findLoopPositions initial =
    Set.size $
        go $
            PathState
                { currentState = initial
                , turnPoints = Seq.empty
                , loopPositions = Set.empty
                }
  where
    go state = case stepWithTurns state of
        Nothing ->
            let positions = loopPositions state
             in trace ("Loop positions: " ++ show (Set.toList positions)) positions
        Just newState ->
            if Set.size (loopPositions newState) > Set.size (loopPositions state)
                then trace ("Found new loop position: " ++ show (Set.difference (loopPositions newState) (loopPositions state))) $ go newState
                else go newState

-- Modified main function to solve part 2
solvePart2 :: String -> Maybe Int
solvePart2 input = do
    initialState <- parseGrid input
    pure $ findLoopPositions initialState

main :: IO ()
main = do
    input <- readFile "test.txt"
    case solvePart2 input of
        Nothing -> putStrLn "Invalid input format"
        Just result -> print result
