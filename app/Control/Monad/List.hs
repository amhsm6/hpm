module Control.Monad.List where

import Control.Monad.IO.Class
import Data.List (singleton)

newtype ListT m a = ListT { runListT :: m [a] }

instance Monad m => Monad (ListT m) where
    return = pure
    m >>= g = ListT $ runListT m >>= \xs -> concat <$> mapM (runListT . g) xs

instance Monad m => Applicative (ListT m) where
    pure x = ListT $ pure [x]
    m1 <*> m2 = m1 >>= \f -> m2 >>= pure . f

instance Monad m => Functor (ListT m) where
    fmap f m = m >>= pure . f

instance MonadIO m => MonadIO (ListT m) where
    liftIO io = ListT $ singleton <$> liftIO io

fromList :: Monad m => [a] -> ListT m a
fromList = ListT . pure

empty :: Monad m => ListT m a
empty = fromList []
