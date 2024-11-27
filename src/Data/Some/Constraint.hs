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
--
-- @
-- AllC '[]       k = ()
-- AllC (c ': cs) k = (c k, AllC cs k)
-- @
type AllC :: forall {k}. [k -> Constraint] -> k -> Constraint
type family AllC cs k :: Constraint where
  AllC '[]       k = ()
  AllC (c ': cs) k = (c k, AllC cs k)

-- | Existential with 'Constraint's.
data SomeCs cs where
  Some :: forall
    (cs :: [Type -> Constraint])
    (a :: Type).
    AllC cs a => a -> SomeCs cs

-- | Alias for 'SomeCs' with just one 'Constraint'.
type Some c = SomeCs '[c]

-- | Existential for containers with 'Constraint's.
data Some1Cs csf csa where
  Some1 :: forall
    k
    (csf :: [(k -> Type) -> Constraint])
    (csa :: [k -> Constraint])
    (f :: k -> Type)
    (a :: k).
    (AllC csf f, AllC csa a) => f a -> Some1Cs csf csa

-- | Alias for 'Some1Cs' with just one 'Constraint'.
type Some1 ca cf = Some1Cs '[ca] '[cf]
