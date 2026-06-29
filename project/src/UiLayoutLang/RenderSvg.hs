module UiLayoutLang.RenderSvg
  ( renderSvg
  ) where

import UiLayoutLang.Types
    ( Resolved(rChildren, rColor, rx, ry, rw, rh),
      Program(programHeight, programWidth) )

renderSvg :: Program -> Resolved -> String
renderSvg program resolved =
  unlines
    [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" ++ show (programWidth program) ++ "\" height=\"" ++ show (programHeight program) ++ "\" viewBox=\"0 0 " ++ show (programWidth program) ++ " " ++ show (programHeight program) ++ "\">"
    , "<rect x=\"0\" y=\"0\" width=\"" ++ show (programWidth program) ++ "\" height=\"" ++ show (programHeight program) ++ "\" fill=\"white\"/>"
    , concatMap renderNode (flatten resolved)
    , "</svg>"
    ]

renderNode :: Resolved -> String
renderNode node =
  "<rect x=\"" ++ show (rx node)
    ++ "\" y=\"" ++ show (ry node)
    ++ "\" width=\"" ++ show (rw node)
    ++ "\" height=\"" ++ show (rh node)
    ++ "\" fill=\"" ++ escapeAttr fill
    ++ "\" stroke=\"#222\" stroke-width=\"1\" fill-opacity=\"" ++ opacity ++ "\"/>\n"
  where
    fill = case rColor node of
      Just color -> color
      Nothing -> "#d9d9d9"
    opacity = case rColor node of
      Just _ -> "0.85"
      Nothing -> "0.18"

flatten :: Resolved -> [Resolved]
flatten node = node : concatMap flatten (rChildren node)

escapeAttr :: String -> String
escapeAttr = concatMap escapeChar
  where
    escapeChar '"' = "&quot;"
    escapeChar '&' = "&amp;"
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar c = [c]
