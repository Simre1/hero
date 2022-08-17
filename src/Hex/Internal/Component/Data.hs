{-# LANGUAGE PatternSynonyms, UnboxedTuples, MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Hex.Internal.Component.Data where

import GHC.Exts

newtype Test = Test (# Int# #)