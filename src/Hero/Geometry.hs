module Hero.Geometry where

import GHC.Generics
import Linear.V2

data Rectangle a = Rectangle {position :: V2 a, size :: V2 a} deriving (Show, Generic)