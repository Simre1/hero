{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Hex.Internal.Component.ComponentId where
import Type.Reflection ( Typeable, SomeTypeRep )
import Language.Haskell.TH
import Data.Data

-- mkComponentId :: SomeTypeRep -> Code IO Int
-- mkComponentId = undefined

-- getComponentAmount :: (Typeable component) => Q Int
-- getComponentAmount = undefined