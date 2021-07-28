{-# OPTIONS_GHC -Wno-missing-signatures #-}

#define LENS_FIELD(name) f x = (\ a -> x { name = a }) <$> f (name x)
