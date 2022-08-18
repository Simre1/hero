{-|
Module      : Hero
Description : An ECS implementation in Haskell focused on performance
Copyright   : (c) Reitinger Simon, 2022
License     : MIT
-}

module Hero
  ( -- * World
    World,
    newWorld,

    -- * Entity
    Entity,

    -- * Component
    Component (..),
    StorableSparseSet,
    storableSparseSet,
    UnboxedSparseSet,
    unboxedSparseSet,
    BoxedSparseSet,
    boxedSparseSet,
    Global,
    makeGlobal,

    -- * System
    System,
    compileSystem,
    cmap,
    cmapM,
    cfold,
    cfoldM,
    cfoldl,
    cfoldr,
    newEntity,
    deleteEntity,
    liftSystem,

    -- * Query
    Query,
    qput,
    qdelete,
    liftQuery,
    runQuery,
    runQuery_,
    singleQuery,
    singleQuery_,
    QCG,
    QCP,
    QCD,
    QCI,

    -- * Re-Exports
    Arrow (..),
    (>>>)
  )
where

import Control.Arrow
import Hero.Component
import Hero.Component.Global
import Hero.Component.SparseSet
import Hero.Entity hiding (newEntity)
import Hero.System
import Hero.World
