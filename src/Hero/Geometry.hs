module Hero.Geometry where

import GHC.Generics
import Linear.V2

-- | Rectangle with a position and a size
data Rectangle a = Rectangle {position :: V2 a, size :: V2 a} deriving (Show, Generic)