-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2012
--
-- -----------------------------------------------------------------------------

-- | Monadic streams
module GHC.Data.Stream (
    Stream(..), yield, liftIO,
    collect, collect_, consume, fromList,
    map, mapM, mapAccumL, mapAccumL_
  ) where

import GHC.Prelude hiding (map, mapAccumL, mapM)

import Control.Monad hiding (mapM)
import Data.Bifunctor (bimap)

-- |
-- @Stream m a b@ is a computation in some Monad @m@ that delivers a sequence
-- of elements of type @a@ followed by a result of type @b@.
--
-- More concretely, a value of type @Stream m a b@ can be run using @runStream@
-- in the Monad @m@, and it delivers either
--
--  * the final result: @Left b@, or
--  * @Right (a,str)@, where @a@ is the next element in the stream, and @str@
--    is a computation to get the rest of the stream.
--
-- Stream is itself a Monad, and provides an operation 'yield' that
-- produces a new element of the stream.  This makes it convenient to turn
-- existing monadic computations into streams.
--
-- The idea is that Stream is useful for making a monadic computation
-- that produces values from time to time.  This can be used for
-- knitting together two complex monadic operations, so that the
-- producer does not have to produce all its values before the
-- consumer starts consuming them.  We make the producer into a
-- Stream, and the consumer pulls on the stream each time it wants a
-- new value.
--
newtype Stream m a b = Stream { runStream :: m (Either b (a, Stream m a b)) }

instance Functor m => Functor (Stream m a) where
  fmap f (Stream m) = Stream (bimap f ((fmap . fmap) f) <$> m)

instance Monad m => Applicative (Stream m a) where
  pure a = Stream (pure (Left a))
  (<*>) = ap

instance Monad m => Monad (Stream m a) where
  Stream m >>= k = Stream $ m >>= \ case
                  Left b        -> runStream (k b)
                  Right (a,str) -> return (Right (a, str >>= k))

yield :: Monad m => a -> Stream m a ()
yield a = Stream (pure (Right (a, pure ())))

liftIO :: IO a -> Stream IO b a
liftIO io = Stream $ Left <$> io

-- | Turn a Stream into an ordinary list, by demanding all the elements.
collect :: Monad m => Stream m a () -> m [a]
collect str = go str []
 where
  go str acc = runStream str >>= \ case
      Left () -> return (reverse acc)
      Right (a, str') -> go str' (a:acc)

-- | Turn a Stream into an ordinary list, by demanding all the elements.
collect_ :: Monad m => Stream m a r -> m ([a], r)
collect_ str = go str []
 where
  go str acc = runStream str >>= \ case
      Left r -> return (reverse acc, r)
      Right (a, str') -> go str' (a:acc)

consume :: Monad m => Stream m a b -> (a -> m ()) -> m b
consume str f = runStream str >>= \ case
      Left ret -> return ret
      Right (a, str') -> do
        f a
        consume str' f

-- | Turn a list into a 'Stream', by yielding each element in turn.
fromList :: Monad m => [a] -> Stream m a ()
fromList = traverse_ yield

-- | Apply a function to each element of a 'Stream', lazily
map :: Functor m => (a -> b) -> Stream m a x -> Stream m b x
map f str = Stream $ runStream str <???> fmap \ (a, str') -> (f a, map f str')

-- | Apply a monadic operation to each element of a 'Stream', lazily
mapM :: Monad m => (a -> m b) -> Stream m a x -> Stream m b x
mapM f str = Stream $ runStream str >>= traverse \ (a, str') -> f a <???> \ b -> (b, mapM f str')

-- | analog of the list-based 'mapAccumL' on Streams.  This is a simple
-- way to map over a Stream while carrying some state around.
mapAccumL :: Monad m => (c -> a -> m (c,b)) -> c -> Stream m a ()
          -> Stream m b c
mapAccumL f c str = Stream $ runStream str >>= \ case
    Left  () -> return (Left c)
    Right (a, str') -> f c a <???> \ (c', b) -> Right (b, mapAccumL f c' str')

mapAccumL_ :: Monad m => (c -> a -> m (c,b)) -> c -> Stream m a r
           -> Stream m b (c, r)
mapAccumL_ f c str = Stream $ runStream str >>= \ case
    Left  r -> return (Left (c, r))
    Right (a, str') -> f c a <???> \ (c', b) -> Right (b, mapAccumL_ f c' str')
