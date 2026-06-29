module Main (main) where

import System.Exit (exitFailure)
import UiLayoutLang.Layout ( resolveProgram )
import UiLayoutLang.Parser ( parseProgram )
import UiLayoutLang.Types
    ( Resolved(..),
      Direction(Leaf, Row, Col),
      Size(..),
      Layout(Layout),
      Program(Program, programTitle, programWidth, programHeight) )

data Test = Test String Bool

main :: IO ()
main = do
  results <- mapM runTest tests
  if and results
    then putStrLn ("Passed " ++ show (length tests) ++ " tests")
    else exitFailure

runTest :: Test -> IO Bool
runTest (Test name ok) =
  if ok
    then putStrLn ("PASS " ++ name) >> return True
    else putStrLn ("FAIL " ++ name) >> return False

tests :: [Test]
tests =
  [ Test "parse example" testParseExample
  , Test "comments are ignored" testComments
  , Test "duplicate property is rejected" testDuplicateProperty
  , Test "two 50 percent children split a row" testRowSplit
  , Test "single 100 percent box fills parent" testFullBox
  , Test "column children have exact positions" testColumnPositions
  , Test "overflow is clipped along row axis" testRowOverflow
  , Test "auto children share remaining row space" testAutoRow
  , Test "end-to-end example positions" testExamplePositions
  , Test "generated layouts stay inside parents" testInsideInvariant
  , Test "generated layouts do not exceed parent main axis" testAxisInvariant
  ]

testParseExample :: Bool
testParseExample =
  case parseProgram exampleInput of
    Right program -> programTitle program == "Main" && programWidth program == 800 && programHeight program == 600
    Left _ -> False

testComments :: Bool
testComments =
  case parseProgram commentsInput of
    Right program -> programWidth program == 100 && programHeight program == 50
    Left _ -> False

testDuplicateProperty :: Bool
testDuplicateProperty =
  case parseProgram duplicateInput of
    Left err -> "duplicate property width" `contains` show err
    Right _ -> False

testRowSplit :: Bool
testRowSplit =
  case parseProgram splitInput of
    Right program ->
      case rChildren (resolveProgram program) of
        [left, right] -> (rx left, rw left, rx right, rw right) == (0, 50, 50, 50)
        _ -> False
    Left _ -> False

testFullBox :: Bool
testFullBox =
  case parseProgram fullBoxInput of
    Right program ->
      case resolveProgram program of
        Resolved Leaf 0 0 120 90 Nothing [] -> True
        _ -> False
    Left _ -> False

testColumnPositions :: Bool
testColumnPositions =
  case parseProgram columnInput of
    Right program ->
      case rChildren (resolveProgram program) of
        [top, middle, bottom] ->
          map rect [top, middle, bottom] == [(0, 0, 200, 25), (0, 25, 100, 50), (0, 75, 200, 25)]
        _ -> False
    Left _ -> False

testRowOverflow :: Bool
testRowOverflow =
  case parseProgram overflowInput of
    Right program ->
      case rChildren (resolveProgram program) of
        [left, right] -> (rw left, rx right, rw right) == (70, 70, 30)
        _ -> False
    Left _ -> False

testAutoRow :: Bool
testAutoRow =
  case parseProgram autoInput of
    Right program ->
      case rChildren (resolveProgram program) of
        [a, b, c] -> map rw [a, b, c] == [34, 33, 33]
        _ -> False
    Left _ -> False

testExamplePositions :: Bool
testExamplePositions =
  case parseProgram exampleInput of
    Right program ->
      case resolveProgram program of
        Resolved Row 0 0 800 600 Nothing [left, right] ->
          (rx left, ry left, rw left, rh left, rColor left) == (0, 0, 160, 600, Just "red")
            && (rx right, ry right, rw right, rh right, rColor right) == (160, 0, 640, 600, Just "blue")
        _ -> False
    Left _ -> False

testInsideInvariant :: Bool
testInsideInvariant =
  all (insideTree . resolveProgram) generatedPrograms

testAxisInvariant :: Bool
testAxisInvariant =
  all (axisTree . resolveProgram) generatedPrograms

insideTree :: Resolved -> Bool
insideTree node =
  all (inside node) (rChildren node) && all insideTree (rChildren node)

inside :: Resolved -> Resolved -> Bool
inside parent child =
  rx child >= rx parent
    && ry child >= ry parent
    && rx child + rw child <= rx parent + rw parent
    && ry child + rh child <= ry parent + rh parent

axisTree :: Resolved -> Bool
axisTree node =
  axisOk node && all axisTree (rChildren node)

axisOk :: Resolved -> Bool
axisOk node =
  case resolvedDirection node of
    Row -> sum (map rw (rChildren node)) <= rw node
    Col -> sum (map rh (rChildren node)) <= rh node
    Leaf -> null (rChildren node)

rect :: Resolved -> (Int, Int, Int, Int)
rect node = (rx node, ry node, rw node, rh node)

generatedPrograms :: [Program]
generatedPrograms =
  [ Program "Generated" 100 80 layout
  | layout <- generatedLayouts
  ]

generatedLayouts :: [Layout]
generatedLayouts =
  [ Layout Row Auto Auto Nothing children
  | children <- generatedChildren
  ]
  ++
  [ Layout Col Auto Auto Nothing children
  | children <- generatedChildren
  ]

generatedChildren :: [[Layout]]
generatedChildren =
  [ [box w h, box w' h']
  | w <- sizes
  , h <- sizes
  , w' <- sizes
  , h' <- sizes
  ]

box :: Size -> Size -> Layout
box width height =
  Layout Leaf width height Nothing []

sizes :: [Size]
sizes = [Auto, Px 0, Px 30, Px 120, Pct 0.25, Pct 0.5, Pct 1]

contains :: String -> String -> Bool
contains needle haystack =
  any (needle `prefixOf`) (tails haystack)

prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:rest) = xs : tails rest

exampleInput :: String
exampleInput =
  unlines
    [ "window \"Main\" 800 x 600 {"
    , "  row {"
    , "    box { width: 20%, height: 100%, color: red }"
    , "    box { width: 80%, height: 100%, color: blue }"
    , "  }"
    , "}"
    ]

commentsInput :: String
commentsInput =
  unlines
    [ "# leading comment"
    , "window \"Comments\" 100 x 50 {"
    , "  row { // inline comment"
    , "    box { width: 50%, height: 100%, color: \"#ff0000\" }"
    , "    /* block"
    , "       comment */"
    , "    box { width: 50%, height: 100%, color: blue }"
    , "  }"
    , "}"
    ]

duplicateInput :: String
duplicateInput =
  "window \"Bad\" 100 x 100 { box { width: 10, width: 20, height: 10 } }"

splitInput :: String
splitInput =
  "window \"Split\" 100 x 40 { row { box { width: 50%, height: 100% } box { width: 50%, height: 100% } } }"

fullBoxInput :: String
fullBoxInput =
  "window \"Full\" 120 x 90 { box { width: 100%, height: 100% } }"

columnInput :: String
columnInput =
  "window \"Column\" 200 x 100 { col { box { width: 100%, height: 25% } box { width: 50%, height: 50% } box { width: 100%, height: 25% } } }"

overflowInput :: String
overflowInput =
  "window \"Overflow\" 100 x 40 { row { box { width: 70, height: 100% } box { width: 70, height: 100% } } }"

autoInput :: String
autoInput =
  "window \"Auto\" 100 x 40 { row { box { height: 100% } box { height: 100% } box { height: 100% } } }"
