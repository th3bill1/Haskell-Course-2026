module UiLayoutLang.Types
  ( Direction(..)
  , Layout(..)
  , Program(..)
  , Resolved(..)
  , Size(..)
  ) where

data Program = Program
  { programTitle  :: String
  , programWidth  :: Int
  , programHeight :: Int
  , programRoot   :: Layout
  } deriving (Eq, Show)

data Layout = Layout
  { layoutDirection :: Direction
  , layoutWidth     :: Size
  , layoutHeight    :: Size
  , layoutColor     :: Maybe String
  , layoutChildren  :: [Layout]
  } deriving (Eq, Show)

data Size = Px Int | Pct Double | Auto
  deriving (Eq, Show)

data Direction = Row | Col | Leaf
  deriving (Eq, Show)

data Resolved = Resolved
  { resolvedDirection :: Direction
  , rx                :: Int
  , ry                :: Int
  , rw                :: Int
  , rh                :: Int
  , rColor            :: Maybe String
  , rChildren         :: [Resolved]
  } deriving (Eq, Show)
