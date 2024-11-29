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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Data.Some.Constraint
(
  -- * Combining constraints
  AllC

  -- * Existentials
  -- ** Flat existentials
  , Somes(..), Some

  -- ** Containerized existentials
  -- *** Constrained containers
  , Somes1(..), Some1

  -- *** Fixed containers
  , SomesF, SomeF
  , mapSome, (<~$>)
  , traverseSome, (<~*>)
) where

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

instance {-# OVERLAPPING #-} Show (Somes (Show ': cs)) where
  showsPrec d (Some x) = showParen (d > 10) $ showString "Some " . showsPrec 11 x

instance {-# OVERLAPPABLE #-} Show (Somes cs) => Show (Somes (c ': cs)) where
  showsPrec d (Some x) = showsPrec d (Some @cs x)

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

-- | Alias for 'Somes1' with a container @f@ and multiple 'Constraint's @csa@ for its elements.
type SomesF f csa = Somes1 '[(~) f] csa

-- | Alias for 'SomeF' with just one 'Constraint' for its elements.
type SomeF f c = SomesF f '[c]

-- | Natural transformation of one container to another.
mapSome :: (forall a. AllC csa a => f a -> g a) -> SomesF f csa -> SomesF g csa
mapSome f (Some1 x) = Some1 (f x)

infixl 4 <~$>
-- | Infix version of 'mapSome'.
(<~$>) :: (forall a. AllC csa a => f a -> g a) -> SomesF f csa -> SomesF g csa
(<~$>) = mapSome

-- | Natural transformation of one container to another - with side effects in @m@.
traverseSome :: Functor m => (forall a. AllC csa a => f a -> m (g a)) -> SomesF f csa -> m (SomesF g csa)
traverseSome f (Some1 x) = Some1 <$> f x

infixl 4 <~*>
-- | Infix version of 'traverseSome'.
(<~*>) :: Functor m => (forall a. AllC csa a => f a -> m (g a)) -> SomesF f csa -> m (SomesF g csa)
(<~*>) = traverseSome

instance {-# OVERLAPPING #-} (forall a. Show a => Show (f a)) => Show (SomesF f (Show ': cs)) where
  showsPrec d (Some1 x) = showParen (d > 10) $ showString "Some " . showsPrec 11 x

instance {-# OVERLAPPABLE #-} Show (SomesF f cs) => Show (SomesF f (c ': cs)) where
  showsPrec d (Some1 x) = showsPrec d (Some1 @_ @('[(~) f]) @cs x)
