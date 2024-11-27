{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Data.Some.Constraint where

import Data.Kind

-- | AllC ensures that a list of 'Constraint's is applied to a poly-kinded 'Type' @k@.
type AllC :: forall k. [k -> Constraint] -> k -> Constraint
type family AllC cs k :: Constraint where
  AllC '[]       k = ()
  AllC (c ': cs) k = (c k, AllC cs k)

-- | Existential with 'Constraint's.
--
-- ==== __Example__
--
-- @
-- someShowableOrd :: 'Somes' '['Show', 'Ord']
-- someShowableOrd = 'Some' ('mempty' :: ['Double'])
-- @
data Somes cs where
  Some :: forall
    (cs :: [Type -> Constraint])
    (a :: Type).
    AllC cs a => a -> Somes cs

-- | Alias for 'Somes' with just one 'Constraint'.
type Some c = Somes '[c]

-- | Existential for containers with 'Constraint's.
--
-- ==== __Example__
--
-- @
-- someNumFunctor :: 'Somes1' '['Functor'] '['Num']
-- someNumFunctor = 'Some1' $ [1, 2, 3 :: 'Int']
-- @
data Somes1 csf csa where
  Some1 :: forall
    k
    (csf :: [(k -> Type) -> Constraint])
    (csa :: [k -> Constraint])
    (f :: k -> Type)
    (a :: k).
    (AllC csf f, AllC csa a) => f a -> Somes1 csf csa

-- | Alias for 'Somes1' with just one 'Constraint'.
type Some1 ca cf = Somes1 '[ca] '[cf]
