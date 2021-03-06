{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE ViewPatterns      #-}

module Form where


-- module Form (
--     formIO
--   , formElems
--   , Person(..)
--   ) where

-- import           Control.Alternative.Free
-- import           Data.Functor.Foldable
import           Control.Alternative.Free.Lawless
import           Control.Applicative
import           Control.Applicative.Lift
import           Control.Monad
import           Control.Monad.Free               as FM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Bool
import           Data.Finite
import           Data.Foldable
import           Data.Function
import           Data.Functor.Compose
import           Data.Functor.Coyoneda
import           Data.Kind
import           Data.List
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Tuple
import           GHC.Generics                     (Generic)
import           Numeric.Natural
import           System.Console.Haskeline
import           System.IO
import           Text.Printf
import           Text.Read
import qualified Data.Aeson                       as A
import qualified Data.List.NonEmpty               as NE
import qualified Data.Text                        as T
import qualified Data.Vector.Sized                as SV
import qualified Data.Yaml                        as Y


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
                , pAge        :: Natural
                , pHeight     :: Maybe Double
                , pFavColor   :: Either Color String
                , pCoffeeTemp :: Temp
                }
  deriving Show

personForm :: Form Person
personForm = P <$> text "Name" ""
               <*> input "Age" Nothing
               -- <*> optional (input "Age" Nothing)
               <*> optional (input "Height" Nothing)
               -- <*> (Left <$> select "Favorite Color" [Blue, Red, Yellow, Green] Nothing)
               <*> (Left <$> select "Favorite Color" [Blue, Red, Yellow, Green] Nothing
                     <|> Right <$> string "Other color" ""
                   )
               <*> check "Ice in Coffee?" Hot Cold False

data IOForm a = IOF { runIOForm :: InputT IO (Either [String] a) }
  deriving (Functor)

instance Applicative IOForm where
    pure = IOF . pure . Right
    -- short circuiting
    -- ioF <*> ioX = IOF $ do
    --   f <- runIOForm ioF
    --   case f of
    --     Left eF  -> return $ Left eF
    --     Right f' -> do
    --       x <- runIOForm ioX
    --       case x of
    --         Left eX  -> return $ Left eX
    --         Right x' -> return $ Right (f' x')
    -- validation-style errors seem to duplicate actions for some reason
    -- that's because it follows every possible branch, nice
    ioF <*> ioX = IOF $ do
      f <- runIOForm ioF
      x <- runIOForm ioX
      case (f, x) of
        (Left eF , Left eX ) -> return $ Left (eF ++ eX)
        (Left eF , Right _ ) -> return $ Left eF
        (Right _ , Left eX ) -> return $ Left eX
        (Right f', Right x') -> return $ Right (f' x')

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

data ElemTree = ETLeaf     T.Text
              | ETSequence [ElemTree]
              | ETChoice   [ElemTree]
  deriving (Show, Eq, Generic)

instance A.ToJSON ElemTree

data ElemTreeF a = ETF { getETF :: ElemTree }
  deriving (Functor, Show)

instance Applicative ElemTreeF where
    pure _ = ETF $ ETSequence []
    (<*>) = \case
      ETF (ETSequence []) -> \case
        ETF y -> ETF y
      ETF (ETSequence xs) -> \case
        ETF (ETSequence ys) -> ETF (ETSequence (xs ++ ys))
        ETF y -> ETF (ETSequence (xs ++ [y]))
      ETF x -> \case
        ETF (ETSequence []) -> ETF x
        ETF (ETSequence ys) -> ETF (ETSequence (x : ys))
        ETF y -> ETF (ETSequence [x, y])

instance Alternative ElemTreeF where
    empty = ETF $ ETChoice []
    (<|>) = \case
      ETF (ETChoice []) -> \case
        ETF y -> ETF y
      ETF (ETChoice xs) -> \case
        ETF (ETChoice ys) -> ETF (ETChoice (xs ++ ys))
        ETF y -> ETF (ETChoice (xs ++ [y]))
      ETF x -> \case
        ETF (ETChoice []) -> ETF x
        ETF (ETChoice ys) -> ETF (ETChoice (x : ys))
        ETF y -> ETF (ETChoice [x,y])

formElems :: Form a -> ElemTree
formElems = getETF . runAlt (ETF . go)
  where
    go :: FormElem b -> ElemTree
    go (FE _ desc _ _) = ETLeaf desc

-- is this lawful?
--
-- pure f <*> ETF (ETLeaf t)
-- = ETF (ETSequence []) <*> ETF (ETLeaf t)
-- = ETF (ETLeaf t)
-- f <$> ETF (ETLeaf t)
-- = ETF (ETLeaf t)
--
-- Okay so now just need associativity...
--
-- (ETF x <|> ETF y) <|> ETF z
-- x = Leaf
--   y = Leaf
--     z = Leaf
--     = (ETF (ETLeaf x) <|> ETF (ETLeaf y)) <|> ETF (ETLeaf z)
--     = ETF (ETChoice [ETLeaf x, ETLeaf y]) <|> ETF (ETLeaf z)
--     = ETF (ETChoice [ETLeaf x, ETLeaf y, ETLeaf z])
--     and
--     = ETF (ETLeaf x) <|> (ETF (ETLeaf y) <|> ETF (ETLeaf z))
--     = ETF (ETLeaf x) <|> ETF (ETChoice [ETLeaf y, ETLeaf z])
--     = ETChoice [ETLeaf x, ETLeaf y, ETLeaf z]
--
-- x = Leaf
--   y = Leaf
--     z = Choice
--     = (ETF (ETLeaf x) <|> ETF (ETLeaf y)) <|> ETF (ETChoice zs)
--     = ETF (ETChoice [ETLeaf x, ETLeaf y]) <|> ETF (ETChoice zs)
--     = ETF (ETChoice (ETLeaf x : ETLeaf y : zs))
--     and
--     = ETF (ETLeaf x) <|> (ETF (ETLeaf y) <|> ETF (ETChoice zs))
--     = ETF (ETLeaf z) <|> ETF (ETChoice (ETLeaf y : zs))
--     = ETF (ETChoice (ETLeaf x : ETLeaf y : zs))
--
-- x = Choice
--   y = Leaf
--     z = Choice
--     = (ETF (ETChoice xs) <|> ETF (ETLeaf y)) <|> ETF (ETChoice zs)
--     = ETF (ETChoice (xs ++ [y])) <|> ETF (ETChoice zs)
--     = ETF (ETChoice (xs ++ y : zs))
--     and
--     = ETF (ETChoice xs) <|> (ETF (ETLeaf y) <|> ETF (ETChoice zs))
--     = ETF (ETChoice xs) <|> ETF (ETChoice (y : zs))
--     = ETF (ETChoice (xs ++ y : zs))
--
-- okay i'm satisfied.
