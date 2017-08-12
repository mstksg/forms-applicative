{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeInType     #-}

module Control.Alternative.Free.Catch (
    Alt
  , liftAlt
  , runAlt
  ) where

import           Control.Applicative
import           Data.Kind

data Alt :: (Type -> Type) -> Type -> Type where
    Pure  :: a -> Alt f a
    Lift  :: f a -> Alt f a
    Empty :: Alt f a
    Ap    :: Alt f (a -> b) -> Alt f a -> Alt f b
    Plus  :: Alt f a -> Alt f a -> Alt f a

liftAlt :: f a -> Alt f a
liftAlt = Lift

instance Functor f => Functor (Alt f) where
    fmap f = \case
      Pure x     -> Pure (f x)
      Lift x     -> Lift (f <$> x)
      Empty      -> Empty
      Ap xs ys   -> Ap ((f .) <$> xs) ys
      Plus xs ys -> Plus (f <$> xs) (f <$> ys)

instance Functor f => Applicative (Alt f) where
    pure  = Pure
    (<*>) = Ap

instance Functor f => Alternative (Alt f) where
    empty = Empty
    (<|>) = Plus

-- must make sure that this follows Applicative laws.

-- Yes now we must quotient this.  Obviously not ideal.
--
-- Quotient with: Ap (Pure f) xs = fmap f xs
-- 
-- meaning: Ap (Pure f) (Pure x)     = Pure (f x)
--          Ap (Pure f) (Lift x)     = Pure (fmap f x)
--          Ap (Pure f) Empty        = Empty
--          Ap (Pure f) (Ap xs ys)   = Ap ((f .) <$> xs) ys
--          Ap (Pure f) (Plus xs ys) = Plus (f <$> xs) (f <$> ys)
--
-- Quotient with: Ap fs (Pure x) = fmap ($ x) fs
--
-- meaning: Ap (Pure f)     (Pure x) = Pure (f x)
--          Ap (Lift f)     (Pure x) = Lift (($ x) <$> f)
--          Ap Empty        (Pure x) = Empty
--          Ap (Ap fs gs)   (Pure x) = Ap (($ x) <$> fs) gs
--          Ap (Plus xs ys) (Pure x) = Plus (($ x) <$> xs) (($ x) <$> ys)
--
-- (x <**> y) <**> z == x <**> (y <**> z)
--
-- quotient with: Ap (Ap fs xs) ys = Ap fs (Ap xs ys)   -- or something
--
--
-- Actually, i think this should be okay as long as we only allow runAlt:
--
-- So we can't export Alt's constructors, only runAlt.

runAlt :: Alternative g => (forall x. f x -> g x) -> Alt f a -> g a
runAlt r = \case
    Pure x     -> pure x
    Lift x     -> r x
    Empty      -> empty
    Ap fs xs   -> runAlt r fs <*> runAlt r xs
    Plus xs ys -> runAlt r xs <|> runAlt r ys

--     (<*>) = \case
--       Pure f -> fmap f
--         -- Pure x    -> Pure (f x)
--         -- Ap x xs   -> Ap x ((f .) <$> xs)
--         -- Empty     -> Empty
--         -- Plus x xs -> Plus (f <$> x) (f <$> xs)
--       -- um does this assume right distribution?
--       Ap f fs -> \x -> Ap f (flip <$> fs <*> x)
--       Empty -> const Empty
--       Plus fs gs -> \case
--         Pure x -> Plus (($ x) <$> fs) (($ x) <$> gs)
--         -- Empty -> Ap Empty (Plus fs gs)
--       --   Pure x -> ($ x) <$> Plus f fs
--       --   Ap x xs -> _
--       --   Empty -> Empty
--         -- Plus x xs -> _


-- data Alt :: (Type -> Type) -> Type -> Type where
--     Pure  :: a -> Alt f a
--     Ap    :: f a -> Alt f (a -> b) -> Alt f b
--     Empty :: Alt f a
--     Plus  :: f a -> Alt f a -> Alt f a

-- liftAlt :: f a -> Alt f a
-- liftAlt x = Ap x (Pure id)

-- instance Functor f => Functor (Alt f) where
--     fmap f = \case
--       Pure x    -> Pure (f x)
--       Ap x xs   -> Ap x ((f .) <$> xs)
--       Empty     -> Empty
--       Plus x xs -> Plus (f <$> x) (f <$> xs)

-- instance Functor f => Applicative (Alt f) where
--     pure = Pure
--     (<*>) = \case
--       Pure f -> fmap f
--         -- Pure x    -> Pure (f x)
--         -- Ap x xs   -> Ap x ((f .) <$> xs)
--         -- Empty     -> Empty
--         -- Plus x xs -> Plus (f <$> x) (f <$> xs)
--       Ap f fs -> \x -> Ap f (flip <$> fs <*> x)
--       Empty -> const Empty
--       Plus f fs -> \case
--         Pure x -> ($ x) <$> Plus f fs
--         Ap x xs -> _
--         Empty -> Empty
--         -- Plus x xs -> _

--         -- Pure x -> Ap f ((\g y -> g y x) <$> fs)
--         -- x@(Ap _ _) -> Ap f (flip <$> fs <*> x)


-- newtype Alt f a = Alt { alternatives :: [AltF f a] }
--   deriving Functor

-- data AltF :: (* -> *) -> * -> * where
--     Ap :: f a -> Alt f (a -> b) -> AltF f b
--     Pure :: a -> AltF f a

-- instance Functor (AltF f) where
--     fmap f = \case
--       Ap x y -> Ap x (fmap (f .) y)
--       Pure x -> Pure (f x)
