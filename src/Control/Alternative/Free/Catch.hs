{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeInType #-}

module Control.Alternative.Free.Catch (
) where

import           Control.Applicative
import           Data.Kind

data Alt :: (Type -> Type) -> Type -> Type where
    Pure  :: a -> Alt f a
    Ap    :: f a -> Alt f (a -> b) -> Alt f b
    Empty :: Alt f a
    Plus  :: f a -> Alt f a -> Alt f a

liftAlt :: f a -> Alt f a
liftAlt x = Ap x (Pure id)

instance Functor f => Functor (Alt f) where
    fmap f = \case
      Pure x    -> Pure (f x)
      Ap x xs   -> Ap x ((f .) <$> xs)
      Empty     -> Empty
      Plus x xs -> Plus (f <$> x) (f <$> xs)

instance Functor f => Applicative (Alt f) where
    pure = Pure
    (<*>) = \case
      Pure f -> fmap f
      Ap f fs -> \x -> Ap f (flip <$> fs <*> x)
      Empty -> const Empty
      -- Plus f fs -> \case
      --   Pure x -> ($ x) <$> Plus f fs
      --   Ap x xs -> _
      --   Empty -> Empty
        -- Plus x xs -> _

instance Functor f => Alternative (Alt f) where
    empty = Empty
    (<|>) = \case
      -- left catch
      Pure x -> const $ Pure x
      -- Ap x xs -> \case
      --   Pure y -> Plus 
      Empty -> const Empty
      Plus x xs -> \y -> Plus x (xs <|> y)

    --   Pure f -> fmap f
    --     -- Pure x    -> Pure (f x)
    --     -- Ap x xs   -> Ap x ((f .) <$> xs)
    --     -- Empty     -> Empty
    --     -- Plus x xs -> Plus (f <$> x) (f <$> xs)
    --   Ap f fs -> \x -> Ap f (flip <$> fs <*> x)
    --   Empty -> const Empty
    --   -- Plus f fs -> \case
    --   --   Pure x -> ($ x) <$> Plus f fs
    --   --   Ap x xs -> _
    --   --   Empty -> Empty
    --     -- Plus x xs -> _
