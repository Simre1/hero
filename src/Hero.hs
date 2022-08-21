-- |
-- Module      : Hero
-- Description : An ECS implementation in Haskell focused on performance
-- Copyright   : (c) Reitinger Simon, 2022
-- License     : MIT
module Hero
  ( -- * World
    World,
    createWorld,

    -- * Entity
    Entity,

    -- * Component
    Component (..),

    -- ** Premade components
    Position2D (..),
    Rotation2D(..),
    Timer(..),
    timer,
    TimeDelta(..),
    timeDelta,
    timingComponents,

    -- ** Store
    StorableSparseSet,
    storableSparseSet,
    storableSparseSet',
    UnboxedSparseSet,
    unboxedSparseSet,
    unboxedSparseSet',
    BoxedSparseSet,
    boxedSparseSet,
    boxedSparseSet',
    Global,
    getGlobal,
    putGlobal,
    addGlobal,

    -- * System
    System,
    cmap,
    cmap',
    cmapM,
    cmapM',
    cfold,
    cfold',
    cfoldM,
    cfoldM',
    cfoldl,
    cfoldr,
    once,
    forward,
    sdelete,
    sput,
    newEntity,
    deleteEntity,
    liftSystem,
    withSetup,
    withSetup',
    compileSystem,

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
    (>>>),
  )
where

import Control.Arrow
import Hero.Component
import Hero.Component.Basic
import Hero.Component.Store.Global
import Hero.Component.Store.SparseSet
import Hero.Entity hiding (newEntity)
import Hero.System
import Hero.System.ComponentFunctions
import Hero.World
