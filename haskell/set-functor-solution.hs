-- as per https://www.reddit.com/r/haskell/comments/2090x3/ask_rhaskell_why_is_there_no_functor_instance_for/cg18kce/
{-# LANGUAGE ConstraintKinds, TypeFamilies #-}

import Data.Set as Set
import GHC.Exts

class Functor f where
  type FunctorConstraint f :: * -> Constraint
  fmap
    :: (FunctorConstraint f a, FunctorConstraint f b)
    => (a -> b) -> f a -> f b

instance Main.Functor (Set) where
  type FunctorConstraint Set = Ord
  fmap = Set.map

x1 = Main.fmap (+1) (Set.fromList [1,2,3])

-- so fmap works for sets now, but we need to redefine instances for list etc.
-- x2 = Main.fmap (+1) [1,2,3]
-- • No instance for (Main.Functor [])

instance Main.Functor ([]) where
  type FunctorConstraint [] = Ord
  fmap = Prelude.map

x2 = Main.fmap (+1) [1,2,3]
