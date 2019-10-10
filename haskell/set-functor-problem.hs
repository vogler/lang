import Data.Set as Set

-- lists can be mapped over
x1 = fmap (+1) [1,2,3]
-- but for sets we have to use a different function b/c of the Ord constraint :(
-- x2 = fmap (+1) (Set.fromList [1,2,3])
-- • No instance for (Functor Set) arising from a use of ‘fmap’
x2 = Set.map (+1) (Set.fromList [1,2,3])

-- see https://www.reddit.com/r/haskell/comments/2090x3/ask_rhaskell_why_is_there_no_functor_instance_for/
