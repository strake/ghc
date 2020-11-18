{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}

module TyFamInj1 where

type family F a b = c | c a -> b, c b -> a where
    F False False = False
    F False True  = True
    F True  False = True
    F True  True  = False

type family Map f as = bs | f bs -> as, as bs -> f where
    Map f '[] = '[]
    Map f ('(:) a as) = '(:) (f a) (Map f as)
