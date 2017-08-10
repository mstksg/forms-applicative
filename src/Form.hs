{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType        #-}

module Form (
    formIO
  , Person(..)
  ) where

import           Control.Alternative.Free
import           Control.Applicative
import           Control.Applicative.Lift
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Bool
import           Data.Finite
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Functor.Coyoneda
import           Data.Kind
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Tuple
import           Numeric.Natural
import           System.Console.Haskeline
import           System.IO
import           Text.Printf
import           Text.Read
import qualified Data.Text                 as T
import qualified Data.Vector.Sized         as SV


dup :: a -> (a, a)
dup x = (x, x)

ifNull :: T.Text -> T.Text -> T.Text
ifNull d x | T.null x  = d
           | otherwise = x

data Choice n = NoChoice | Selected (Finite n)

data Elem :: Type -> Type where
    EText   :: Elem T.Text
    ESelect :: Sing n -> SV.Vector n T.Text -> Elem (Choice n)
    ECheck  :: Elem Bool

data FormElem :: Type -> Type where
    FE :: { feElem    :: Elem a
          , feDescr   :: T.Text
          , feInit    :: a
          , feParse   :: a -> Either String (b, T.Text)
          }
       -> FormElem b

instance Functor FormElem where
    fmap f (FE e ds df p) = FE e ds df ((fmap . first) f . p)

type Form = Alt FormElem

-- Sample Form primitives

text
    :: T.Text         -- ^ description
    -> T.Text         -- ^ initial
    -> Form T.Text
text descr d = liftAlt $ FE EText descr d (Right . dup)

string
    :: T.Text         -- ^ description
    -> String         -- ^ initial
    -> Form String
string descr d = T.unpack <$> text descr (T.pack d)

input'
    :: (T.Text -> Either String a)  -- ^ read
    -> (a -> T.Text)                -- ^ show
    -> T.Text                       -- ^ description
    -> Maybe a                      -- ^ default
    -> Form a
input' r s descr d = liftAlt $ FE EText descr (maybe "" s d) (fmap (second s . dup) . r . p)
  where
    p = maybe id (ifNull . s) d

input
    :: (Read a, Show a)
    => T.Text
    -> Maybe a
    -> Form a
input = input' r (T.pack . show)
  where
    r = maybe (Left "Could not parse") Right . readMaybe . T.unpack

selectV'
    :: KnownNat n
    => T.Text
    -> SV.Vector n (a, T.Text)
    -> Choice n
    -> Form a
selectV' descr xs d = liftAlt $ FE (ESelect SNat (snd <$> xs)) descr d p
  where
    p NoChoice     = Left "No selection"
    p (Selected i) = Right (xs `SV.index` i)

selectV
    :: (KnownNat n, Show a)
    => T.Text
    -> SV.Vector n a
    -> Choice n
    -> Form a
selectV descr xs = selectV' descr ((\x -> (x, T.pack (show x))) <$> xs)

select'
    :: T.Text
    -> [(a, T.Text)]
    -> Maybe Integer
    -> Form a
select' descr xs d = SV.withSizedList xs $ \ys ->
    selectV' descr ys (maybe NoChoice Selected . (packFinite =<<) $ d)

select
    :: Show a
    => T.Text
    -> [a]
    -> Maybe Integer
    -> Form a
select descr xs = select' descr ((\x -> (x, T.pack (show x))) <$> xs)

check'
    :: T.Text
    -> (a, T.Text)  -- ^ "off" value
    -> (a, T.Text)  -- ^ "on" value
    -> Bool         -- ^ initial
    -> Form a
check' descr x y d = liftAlt $
    FE ECheck descr d (Right . bool x y)

check
    :: Show a
    => T.Text
    -> a            -- ^ "off" value
    -> a            -- ^ "on" value
    -> Bool         -- ^ initial
    -> Form a
check descr x y = check' descr (x, T.pack (show x)) (y, T.pack (show y))

-- A Form to Parse

data Color = Blue | Red | Yellow | Green
    deriving Show

data Temp = Hot | Cold
    deriving Show

data Person = P { pName       :: T.Text
                , pAge        :: Maybe Natural
                , pFavColor   :: Either Color String
                , pCoffeeTemp :: Temp
                }
  deriving Show

personForm :: Form Person
personForm = P <$> text "Name" ""
               <*> optional (input "Age" Nothing)
               <*> (Left <$> select "Favorite Color" [Blue, Red, Yellow, Green] Nothing
                     <|> Right <$> string "Other color" ""
                   )
               <*> check "Ice in Coffee?" Hot Cold False

data IOForm a = IOF { runIOForm :: InputT IO (Either [String] a) }
  deriving (Functor)

instance Applicative IOForm where
    pure = IOF . pure . Right
    ioF <*> ioX = IOF $ do
      f <- runIOForm ioF
      case f of
        Left eF  -> return $ Left eF
        Right f' -> do
          x <- runIOForm ioX
          case x of
            Left eX  -> return $ Left eX
            Right x' -> return $ Right (f' x')
    -- validation-style errors seem to duplicate actions for some reason
    -- ioF <*> ioX = IOF $ do
    --   f <- runIOForm ioF
    --   x <- runIOForm ioX
    --   case (f, x) of
    --     (Left eF , Left eX ) -> return $ Left (eF ++ eX)
    --     (Left eF , Right _ ) -> return $ Left eF
    --     (Right _ , Left eX ) -> return $ Left eX
    --     (Right f', Right x') -> return $ Right (f' x')

instance Alternative IOForm where
    empty = IOF $ pure (Left [])
    ioX <|> ioY = IOF $ do
      x <- runIOForm ioX
      case x of
        Right x' -> return $ Right x'
        Left eX  -> do
          y <- runIOForm ioY
          case y of
            Left eY  -> return $ Left (eX ++ eY)
            Right y' -> return $ Right y'

formIO :: Form a -> IO (Either [String] a)
formIO = runInputT defaultSettings . runIOForm . runAlt (IOF . go)
  where
    go :: FormElem b -> InputT IO (Either [String] b)
    go (FE e desc d p) = do
      x <- case e of
        EText  -> Right . maybe "" T.pack <$> getInputLineWithInitial (printf "%s: " desc) (T.unpack d, "")
        ESelect SNat xs -> do
          liftIO $ do
            printf "%s\n" desc
            forM_ (SV.indexed xs) . uncurry $ \i t ->
              printf "(%d) %s\n" (i + 1) t
          let cDef = case d of
                       NoChoice   -> ""
                       Selected i -> show (getFinite i + 1)
          c <- getInputLineWithInitial "Item Number: " (cDef, "")
          return $ case c of
            Nothing -> Right NoChoice
            Just c' -> case readMaybe c' of
              Nothing -> Left "Could not parse item number"
              Just i -> case packFinite (i - 1) of
                Nothing -> Left "Selection out of range."
                Just j  -> Right $ Selected j
        ECheck -> do
          let offVal = either (const "???") snd $ p False
              onVal  = either (const "???") snd $ p True
              prompt = printf "%s: " desc ++ case d of
                        False -> printf "([1] %s / 2 %s) " offVal onVal
                        True  -> printf "(1 %s / [2] %s) " offVal onVal
          c <- getInputChar prompt
          case c of
            Just '1' -> return $ Right False
            Just '2' -> return $ Right True
            _        -> return $ Right d
      case p =<< x of
        Left err      -> return $ Left [err]
        Right (x', s) -> do
          liftIO $ printf "Parsed: %s\n" s
          return $ Right x'
    -- ECheck  :: Elem Bool
          -- case x of
          -- _
      -- printf "%s: " desc
      -- rp
      -- _

      -- printf "%s: " desc
      -- case
      -- forM_ d $ \d ->
      --   printf "(%s) " d
      -- _
    -- FE :: { feElem    :: Elem a
    --       , feDescr   :: T.Text
    --       , feDefault :: Maybe a
    --       , feParse   :: a -> Either String (b, T.Text)
    --       }
    --    -> FormElem b

--     EText   :: Elem T.Text
--     ESelect :: Sing n -> SV.Vector n T.Text -> Elem (Choice n)
--     ECheck  :: Elem Bool
