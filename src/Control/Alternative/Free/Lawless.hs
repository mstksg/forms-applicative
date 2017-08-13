{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE TypeInType     #-}

module Control.Alternative.Free.Lawless (
    Alt
  , liftAlt
  , runAlt
  ) where

import           Control.Applicative
import           Data.Foldable
import           Data.Kind
import           Data.Profunctor

data ApChain :: (Type -> Type) -> Type -> Type -> Type where
    ACNil :: (a -> b) -> ApChain f a b
    (:<~) :: f (b -> c) -> ApChain f a b -> ApChain f a c

infixr 5 :<~

instance Functor f => Functor (ApChain f r) where
    fmap f = \case
      ACNil g  -> ACNil (f . g)
      x :<~ xs -> fmap (f .) x :<~ xs

instance Functor f => Profunctor (ApChain f) where
    rmap = fmap
    lmap f = \case
      ACNil g  -> ACNil (g . f)
      x :<~ xs -> x :<~ lmap f xs
    dimap f g = \case
      ACNil h  -> ACNil (g . h . f)
      x :<~ xs -> fmap (g .) x :<~ lmap f xs

instance Functor f => Strong (ApChain f) where
    first' = \case
      ACNil g  -> ACNil (first' g)
      x :<~ xs -> fmap first' x :<~ first' xs
    second' = \case
      ACNil g  -> ACNil (second' g)
      x :<~ xs -> fmap second' x :<~ second' xs

appendAC :: Functor f => ApChain f b c -> ApChain f a b -> ApChain f a c
appendAC = \case
    ACNil f -> \case
      ACNil g -> ACNil (f . g)
      g :<~ gs -> ((f .) <$> g) :<~ gs
    x :<~ xs -> \ys ->
      x :<~ appendAC xs ys


single :: f (a -> b) -> ApChain f a b
single x = x :<~ ACNil id

(~>:) :: Functor f => ApChain f b c -> f (a -> b) -> ApChain f a c
xs ~>: x = appendAC xs (single x)

data Alt :: (Type -> Type) -> Type -> Type where
    Lift     :: f a -> Alt f a
    Sequence :: ApChain (Alt f) () a -> Alt f a
    Choice   :: [Alt f a] -> Alt f a

instance Functor f => Functor (Alt f) where
    fmap f = \case
      Lift x              -> Lift $ fmap f x
      Sequence (ACNil g)  -> Sequence (ACNil (f . g))
      Sequence (g :<~ gs) -> Sequence ((fmap (f .) g) :<~ gs)
      Choice xs           -> Choice (map (fmap f) xs)

instance Functor f => Applicative (Alt f) where
    pure x = Sequence $ ACNil (const x)
    (<*>)  = \case
      Sequence (ACNil f)  -> fmap (f ())
      Sequence fs ->
        let fs' = dimap ((),) (uncurry ($)) $ first' fs
        in  \case Sequence xs -> Sequence $ appendAC fs' xs
                  Choice xs   -> Sequence $ fs' ~>: (const <$> Choice xs)
      f -> \case
        Sequence (ACNil x) -> ($ x ()) <$> f
        Sequence xs -> Sequence $ f :<~ xs
        x -> Sequence $ f :<~ (const <$> x) :<~ ACNil id

instance Functor f => Alternative (Alt f) where
    empty = Choice []
    (<|>) = \case
      Choice [] -> id
      Choice xs -> \case
        Choice ys -> Choice (xs ++ ys)
        y         -> Choice (xs ++ [y])
      x -> \case
        Choice [] -> x
        Choice ys -> Choice (x : ys)
        y -> Choice [x, y]

    -- (<|>) = \case
    --   ETF (ETChoice []) -> \case
    --     ETF y -> ETF y
    --   ETF (ETChoice xs) -> \case
    --     ETF (ETChoice ys) -> ETF (ETChoice (xs ++ ys))
    --     ETF y -> ETF (ETChoice (xs ++ [y]))
    --   ETF x -> \case
    --     ETF (ETChoice []) -> ETF x
    --     ETF (ETChoice ys) -> ETF (ETChoice (x : ys))
    --     ETF y -> ETF (ETChoice [x,y])

liftAlt :: f a -> Alt f a
liftAlt = Lift

runAlt :: Alternative g => (forall x. f x -> g x) -> Alt f a -> g a
runAlt r = \case
    Lift x -> r x
    Sequence (ACNil x)  -> pure (x ())
    Sequence (x :<~ xs) -> runAlt r x <*> runAlt r (Sequence xs)
    Choice xs -> asum $ map (runAlt r) xs

-- data Alt :: (Type -> Type) -> Type -> Type where
--     Pure  :: a -> Alt f a
--     Lift  :: f a -> Alt f a
--     Empty :: Alt f a
--     Ap    :: Alt f (a -> b) -> Alt f a -> Alt f b
--     Plus  :: Alt f a -> Alt f a -> Alt f a

-- liftAlt :: f a -> Alt f a
-- liftAlt = Lift

-- instance Functor f => Functor (Alt f) where
--     fmap f = \case
--       Pure x     -> Pure (f x)
--       Lift x     -> Lift (f <$> x)
--       Empty      -> Empty
--       Ap xs ys   -> Ap ((f .) <$> xs) ys
--       Plus xs ys -> Plus (f <$> xs) (f <$> ys)

-- instance Functor f => Applicative (Alt f) where
--     pure  = Pure
--     (<*>) = Ap

-- instance Functor f => Alternative (Alt f) where
--     empty = Empty
--     (<|>) = Plus

-- -- We can't export Alt's constructors, only runAlt.

-- runAlt :: Alternative g => (forall x. f x -> g x) -> Alt f a -> g a
-- runAlt r = \case
--     Pure x     -> pure x
--     Lift x     -> r x
--     Empty      -> empty
--     Ap fs xs   -> runAlt r fs <*> runAlt r xs
--     Plus xs ys -> runAlt r xs <|> runAlt r ys

-- --     (<*>) = \case
-- --       Pure f -> fmap f
-- --         -- Pure x    -> Pure (f x)
-- --         -- Ap x xs   -> Ap x ((f .) <$> xs)
-- --         -- Empty     -> Empty
-- --         -- Plus x xs -> Plus (f <$> x) (f <$> xs)
-- --       -- um does this assume right distribution?
-- --       Ap f fs -> \x -> Ap f (flip <$> fs <*> x)
-- --       Empty -> const Empty
-- --       Plus fs gs -> \case
-- --         Pure x -> Plus (($ x) <$> fs) (($ x) <$> gs)
-- --         -- Empty -> Ap Empty (Plus fs gs)
-- --       --   Pure x -> ($ x) <$> Plus f fs
-- --       --   Ap x xs -> _
-- --       --   Empty -> Empty
-- --         -- Plus x xs -> _


-- -- data Alt :: (Type -> Type) -> Type -> Type where
-- --     Pure  :: a -> Alt f a
-- --     Ap    :: f a -> Alt f (a -> b) -> Alt f b
-- --     Empty :: Alt f a
-- --     Plus  :: f a -> Alt f a -> Alt f a

-- -- liftAlt :: f a -> Alt f a
-- -- liftAlt x = Ap x (Pure id)

-- -- instance Functor f => Functor (Alt f) where
-- --     fmap f = \case
-- --       Pure x    -> Pure (f x)
-- --       Ap x xs   -> Ap x ((f .) <$> xs)
-- --       Empty     -> Empty
-- --       Plus x xs -> Plus (f <$> x) (f <$> xs)

-- -- instance Functor f => Applicative (Alt f) where
-- --     pure = Pure
-- --     (<*>) = \case
-- --       Pure f -> fmap f
-- --         -- Pure x    -> Pure (f x)
-- --         -- Ap x xs   -> Ap x ((f .) <$> xs)
-- --         -- Empty     -> Empty
-- --         -- Plus x xs -> Plus (f <$> x) (f <$> xs)
-- --       Ap f fs -> \x -> Ap f (flip <$> fs <*> x)
-- --       Empty -> const Empty
-- --       Plus f fs -> \case
-- --         Pure x -> ($ x) <$> Plus f fs
-- --         Ap x xs -> _
-- --         Empty -> Empty
-- --         -- Plus x xs -> _

-- --         -- Pure x -> Ap f ((\g y -> g y x) <$> fs)
-- --         -- x@(Ap _ _) -> Ap f (flip <$> fs <*> x)


-- -- newtype Alt f a = Alt { alternatives :: [AltF f a] }
-- --   deriving Functor

-- -- data AltF :: (* -> *) -> * -> * where
-- --     Ap :: f a -> Alt f (a -> b) -> AltF f b
-- --     Pure :: a -> AltF f a

-- -- instance Functor (AltF f) where
-- --     fmap f = \case
-- --       Ap x y -> Ap x (fmap (f .) y)
-- --       Pure x -> Pure (f x)
