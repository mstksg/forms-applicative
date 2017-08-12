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

-- import           Data.Functor.Foldable
import           Control.Alternative.Free
import           Control.Applicative
import           Control.Applicative.Lift
import           Control.Monad
import           Control.Monad.Free        as FM
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
import           GHC.Generics              (Generic)
import           Numeric.Natural
import           System.Console.Haskeline
import           System.IO
import           Text.Printf
import           Text.Read
import qualified Data.Aeson                as A
import qualified Data.List.NonEmpty        as NE
import qualified Data.Text                 as T
import qualified Data.Vector.Sized         as SV
import qualified Data.Yaml                 as Y


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
                -- , pHeight     :: Maybe Double
                , pFavColor   :: Either Color String
                , pCoffeeTemp :: Temp
                }
  deriving Show

personForm :: Form Person
personForm = P <$> text "Name" ""
               <*> input "Age" Nothing
               -- <*> optional (input "Age" Nothing)
               -- <*> optional (input "Height" Nothing)
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
    -- that's because it follows every possible branch, nice
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

data ElemTree = ETLeaf     T.Text
              | ETSequence [ElemTree]
              | ETChoice   [ElemTree]
  deriving (Show, Eq, Generic)

normalizeET :: ElemTree -> ElemTree
normalizeET = \case
    ETLeaf x -> ETLeaf x
    ETSequence [x] -> normalizeET x
    ETSequence xs -> ETSequence . filter (not . isEmpty) $ normalizeET <$> xs
    ETChoice [x] -> normalizeET x
    ETChoice xs -> ETChoice $ recombine <$> undistribute f (filter (not . isEmpty) $ normalizeET <$> xs)
    -- case undistribute f (filter (not . isEmpty) xs) of
    --   Just (ys, zs) -> ETSequence $ ETChoice (normalizeET <$> ys) : (normalizeET <$> zs)
    --   Nothing       -> ETChoice $ normalizeET <$> xs
  where
    isEmpty = \case
      ETSequence [] -> True
      ETChoice   [] -> True
      _             -> False
    f = \case
      ETSequence (x:xs) -> Just (x, xs)
      _ -> Nothing
    recombine (Right xs) = normalizeET xs
    recombine (Left (ys, zs)) = ETSequence $ ETChoice (normalizeET <$> ys) : (normalizeET <$> zs)


firstChoice :: ElemTree -> [T.Text]
firstChoice = \case
  ETLeaf x       -> [x]
  ETSequence xs  -> firstChoice =<< xs
  ETChoice []    -> []
  ETChoice (x:_) -> firstChoice x

etString :: ElemTree -> T.Text
etString = \case
  ETLeaf x -> x
  ETSequence [] -> ""
  ETSequence xs -> T.concat ["(", T.intercalate " and " $ etString <$> xs, ")"]
  ETChoice [] -> ""
  ETChoice xs -> T.concat ["(", T.intercalate " or " $ etString <$> xs, ")"]

undistribute :: Eq c => (a -> Maybe (b, c)) -> [a] -> [Either ([b], c) a]
undistribute f = map recombine . groupBy g . map f'
    -- case traverse f xs of
    -- Just yszs -> map recombine $ groupBy ((==) `on` snd) yszs
  where
    f' x = case f x of
      Just y  -> Left y
      Nothing -> Right x
    g (Left (_,x)) (Left (_,y)) = x == y
    g _            _            = False
    recombine [Right x] = Right x
    recombine (Left (x,y) : xys) = Left (x : [ x' | Left (x', _) <- xys ], y)
    recombine _ = undefined
    -- recombine ((x,y):xys) = (x : map fst xys,y)
                      -- do
    -- (y,z):yszs <- traverse f xs
    -- guard $ all ((== z) . snd) yszs
    -- return (y : fmap fst yszs, z)

instance A.ToJSON ElemTree

data ElemTreeF a = ETF { getETF :: ElemTree }
  deriving (Functor, Show)

sequenceET :: ElemTree -> ElemTree -> ElemTree
sequenceET = \case
    x@(ETLeaf _) -> \case
      y@(ETLeaf _)    -> ETSequence [x, y]
      ETSequence ys   -> ETSequence $ x : ys
      ys@(ETChoice _) -> ETSequence [x, ys]
    ETSequence xs -> \case
      y@(ETLeaf _)    -> ETSequence $ xs ++ [y]
      ETSequence ys   -> ETSequence $ xs ++ ys
      ys@(ETChoice _) -> ETSequence $ xs ++ [ys]
    ETChoice xs -> \case
      y@(ETLeaf _)    -> ETSequence $ xs ++ [y]
      ETSequence ys   -> ETSequence $ ETChoice xs : ys
      ys@(ETChoice _) -> ETSequence [ETChoice xs, ys]

instance Applicative ElemTreeF where
    pure _ = ETF $ ETSequence []
    ETF x <*> ETF y = ETF $ sequenceET x y

choiceET :: ElemTree -> ElemTree -> ElemTree
choiceET = \case
    x@(ETLeaf _) -> \case
      y@(ETLeaf _)      -> ETChoice [x, y]
      ys@(ETSequence _) -> ETChoice [x, ys]
      ETChoice ys       -> ETChoice $ x : ys
    ETSequence xs -> \case
      y@(ETLeaf _)      -> ETChoice $ xs ++ [y]
      ys@(ETSequence _) -> ETChoice $ [ETSequence xs, ys]
      ETChoice ys       -> ETChoice $ ETSequence xs : ys
    ETChoice xs -> \case
      y@(ETLeaf _)      -> ETChoice $ xs ++ [y]
      ys@(ETSequence _) -> ETChoice $ xs ++ [ys]
      ETChoice ys       -> ETChoice $ xs ++ ys

instance Alternative ElemTreeF where
    empty = ETF $ ETChoice []
    ETF x <|> ETF y = ETF $ choiceET x y

-- shoe :: ElemTree -> String
-- printElemTree = \case

-- sequenceET :: ElemTree -> ElemTree -> ElemTree
-- sequenceET t1 t2 = ETSequence ()
    -- ETLeaf x -> \case
    --   ETLeaf y -> ETSequence (ETLeaf x NE.:| [ETLeaf y])

-- dephantom :: ElemTree a -> ElemTree b
-- dephantom = \case
--     ETLeaf -> ETLeaf
--     ETSequence xs -> ETSequence (fmap dephantom xs)
--     ETChoice xs   -> ETChoice (fmap dephantom xs)

-- instance Applicative ElemTree where
--     pure _ = ETPure
--     ETLeaf x <*> ETLeaf y      = ETSequence [ETLeaf x, ETLeaf y]
--     ETLeaf x <*> ETSequence ys = ETSequence $ ETLeaf x : _ ys

-- data ElemAlt a = EA { getEA :: Free [] T.Text }
--     deriving Functor

-- instance Applicative ElemAlt where
--     pure _ = EA $ liftF []
--     -- EA xs <*> EA ys = EA . liftF $ retract xs ++ retract ys
--     EA xs <*> EA ys = case (xs, ys) of
--       (FM.Pure x  , FM.Pure y  ) -> EA $ liftF [x,y]
--       (FM.Pure x  , FM.Free ys') -> EA $ FM.Free (liftF [x] : ys')
--       (FM.Free xs', FM.Free ys') -> EA $ FM.Free (xs' ++ ys')
--       (FM.Free xs', FM.Pure y  ) -> EA $ FM.Free (xs' ++ [liftF [y]])

-- instance Alternative ElemAlt where
--     empty = EA $ liftF []
--     -- EA xs <|> EA ys = EA . liftF $ retract xs ++ retract ys
--     EA xs <|> EA ys = case (xs, ys) of
--       (FM.Pure x  , FM.Pure y  ) -> EA $ FM.Free (fmap FM.Pure [x,y])
--       -- (FM.Pure x  , FM.Free ys') -> EA $ FM.Free (liftF [x] : ys')
--       -- (FM.Free xs', FM.Free ys') -> EA $ FM.Free (xs' ++ ys')
--       -- (FM.Free xs', FM.Pure y  ) -> EA $ FM.Free (xs' ++ [liftF [y]])

-- instance Alternative ElemAlt where
--     empty = EA []
--     EA xs <|> EA ys = EA (fmap (T.append "+ ") xs ++ fmap (T.append "- ") ys)

formElems :: Form a -> ElemTree
formElems = getETF . runAlt (ETF . go)
  where
    go :: FormElem b -> ElemTree
    go (FE _ desc _ _) = ETLeaf desc

-- data JSONAlt a = JA { getJA :: A.Value }
--     deriving Functor

-- instance Applicative JSONAlt where
--     pure _ = JA A.Null
--     JA x <*> JA y =

-- formJSON :: Form a -> A.Value
-- formJSON = getJA . runAlt (JA . go)
--   where
--     go :: FormElem b -> A.Value
--     go (FE _ desc _ _) = A.String desc


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

-- anyways looks like a HUGE hassle
-- just to get the right behavior
--
-- what i want is a free alternative that is *not* left distributing
