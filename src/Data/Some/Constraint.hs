{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
type Some1 cf ca = Somes1 '[cf] '[ca]

-- | Alias for 'Somes1' with a container @f@ and multple 'Constraint's @csa@ for its elements.
type SomesF f csa = Somes1 '[(~) f] csa

-- | Alias for 'SomeF' with just one 'Constraint' for its elements.
type SomeF f c = SomesF f '[c]

instance {-# OVERLAPPING #-} Show (Somes (Show ': cs)) where
  showsPrec d (Some x) = showParen (d > 10) $ showString "Some " . showsPrec 11 x

instance {-# OVERLAPPABLE #-} Show (Somes cs) => Show (Somes (c ': cs)) where
  showsPrec d (Some x) = showsPrec d (Some @cs x)
