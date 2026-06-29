module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import UiLayoutLang.Layout ( resolveProgram )
import UiLayoutLang.Parser ( parseProgram )
import UiLayoutLang.RenderSvg ( renderSvg )
import UiLayoutLang.Types
    ( Resolved(rColor, rChildren, resolvedDirection, rx, ry, rw, rh),
      Direction(..) )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    ["--demo"] -> runInput demoProgram Nothing
    ["--demo", "--svg", outPath] -> runInput demoProgram (Just outPath)
    [inPath] -> readFile inPath >>= \input -> runInput input Nothing
    [inPath, "--svg", outPath] -> readFile inPath >>= \input -> runInput input (Just outPath)
    _ -> usage >> exitFailure

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  ui-layout-lang FILE"
  putStrLn "  ui-layout-lang FILE --svg OUT.svg"
  putStrLn "  ui-layout-lang --demo"
  putStrLn "  ui-layout-lang --demo --svg OUT.svg"

runInput :: String -> Maybe FilePath -> IO ()
runInput input svgPath =
  case parseProgram input of
    Left err -> do
      putStrLn ("Parse error: " ++ show err)
      exitFailure
    Right program -> do
      let resolved = resolveProgram program
      putStrLn (formatResolved resolved)
      case svgPath of
        Nothing -> return ()
        Just path -> writeFile path (renderSvg program resolved)

formatResolved :: Resolved -> String
formatResolved = unlines . go 0
  where
    go depth node =
      line depth node : concatMap (go (depth + 1)) (rChildren node)

    line depth node =
      replicate (depth * 2) ' '
        ++ showDirection (resolvedDirection node)
        ++ " x="
        ++ show (rx node)
        ++ " y="
        ++ show (ry node)
        ++ " w="
        ++ show (rw node)
        ++ " h="
        ++ show (rh node)
        ++ colorText (rColor node)

    colorText Nothing = ""
    colorText (Just color) = " color=" ++ color

showDirection :: Direction -> String
showDirection Row = "row"
showDirection Col = "col"
showDirection Leaf = "box"

demoProgram :: String
demoProgram =
  unlines
    [ "window \"Main\" 800 x 600 {"
    , "  row {"
    , "    box { width: 20%, height: 100%, color: red }"
    , "    box { width: 80%, height: 100%, color: blue }"
    , "  }"
    , "}"
    ]
