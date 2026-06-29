module UiLayoutLang.Layout
  ( resolveProgram
  ) where

import UiLayoutLang.Types
    ( Resolved(..),
      Direction(Leaf, Row, Col),
      Size(..),
      Layout(layoutWidth, layoutChildren, layoutDirection, layoutColor,
             layoutHeight),
      Program(programRoot, programWidth, programHeight) )

data Rect = Rect Int Int Int Int

resolveProgram :: Program -> Resolved
resolveProgram program =
  resolveLayout (programRoot program) (Rect 0 0 width height)
  where
    width = clamp 0 (programWidth program) (resolveFill (layoutWidth (programRoot program)) (programWidth program))
    height = clamp 0 (programHeight program) (resolveFill (layoutHeight (programRoot program)) (programHeight program))

resolveLayout :: Layout -> Rect -> Resolved
resolveLayout layout rect@(Rect x y width height) =
  Resolved
    { resolvedDirection = layoutDirection layout
    , rx = x
    , ry = y
    , rw = width
    , rh = height
    , rColor = layoutColor layout
    , rChildren = children
    }
  where
    children = case layoutDirection layout of
      Row -> resolveRow (layoutChildren layout) rect
      Col -> resolveCol (layoutChildren layout) rect
      Leaf -> []

resolveRow :: [Layout] -> Rect -> [Resolved]
resolveRow children (Rect x y width height) =
  go children sizes x width
  where
    sizes = mainSizes layoutWidth children width

    go [] [] _ _ = []
    go (child:rest) (size:moreSizes) currentX remaining =
      let actualWidth = min size remaining
          actualHeight = clamp 0 height (resolveFill (layoutHeight child) height)
          childRect = Rect currentX y actualWidth actualHeight
      in resolveLayout child childRect : go rest moreSizes (currentX + actualWidth) (remaining - actualWidth)
    go _ _ _ _ = []

resolveCol :: [Layout] -> Rect -> [Resolved]
resolveCol children (Rect x y width height) =
  go children sizes y height
  where
    sizes = mainSizes layoutHeight children height

    go [] [] _ _ = []
    go (child:rest) (size:moreSizes) currentY remaining =
      let actualHeight = min size remaining
          actualWidth = clamp 0 width (resolveFill (layoutWidth child) width)
          childRect = Rect x currentY actualWidth actualHeight
      in resolveLayout child childRect : go rest moreSizes (currentY + actualHeight) (remaining - actualHeight)
    go _ _ _ _ = []

mainSizes :: (Layout -> Size) -> [Layout] -> Int -> [Int]
mainSizes getSize children parentSize =
  assign children autoSizes
  where
    fixedSizes = [ resolveFixed size parentSize | child <- children, let size = getSize child, size /= Auto ]
    fixedTotal = sum fixedSizes
    autoCount = length [ () | child <- children, getSize child == Auto ]
    remaining = max 0 (parentSize - fixedTotal)
    autoSizes = splitEvenly remaining autoCount

    assign [] _ = []
    assign (child:rest) autos =
      case getSize child of
        Auto ->
          case autos of
            autoSize : moreAutos -> autoSize : assign rest moreAutos
            [] -> 0 : assign rest []
        size -> resolveFixed size parentSize : assign rest autos

resolveFill :: Size -> Int -> Int
resolveFill Auto parentSize = parentSize
resolveFill size parentSize = resolveFixed size parentSize

resolveFixed :: Size -> Int -> Int
resolveFixed Auto parentSize = parentSize
resolveFixed (Px pixels) _ = max 0 pixels
resolveFixed (Pct fraction) parentSize = floor (fraction * fromIntegral parentSize)

splitEvenly :: Int -> Int -> [Int]
splitEvenly _ 0 = repeat 0
splitEvenly total count =
  [ base + if i < extra then 1 else 0 | i <- [0 .. count - 1] ] ++ repeat 0
  where
    base = total `div` count
    extra = total `mod` count

clamp :: Int -> Int -> Int -> Int
clamp low high value = max low (min high value)
