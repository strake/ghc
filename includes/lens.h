{-# OPTIONS_GHC -Wno-missing-signatures #-}

#define LENS_FIELD(name, field) {-# INLINE name #-}; name f x = (\ a -> x { field = a }) <$> f (field x)
