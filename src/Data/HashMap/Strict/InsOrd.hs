{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
-- | 'InsOrdHashMap' is like 'HashMap', but it folds and traverses in insertion order.
--
-- This module interface mimics "Data.HashMap.Strict", with some additions.
module Data.HashMap.Strict.InsOrd (
    InsOrdHashMap,
    -- * Construction
    empty,
    singleton,
    -- * Basic interface
    null,
    size,
    member,
    lookup,
    lookupDefault,
    insert,
    insertWith,
    delete,
    adjust,
    update,
    alter,
    -- * Combine
    union,
    unionWith,
    unionWithKey,
    unions,
    -- * Transformations
    map,
    mapKeys,
    traverseKeys,
    mapWithKey,
    traverseWithKey,
    -- ** Unordered
    unorderedTraverse,
    unorderedTraverseWithKey,
    -- * Difference and intersection
    difference,
    intersection,
    intersectionWith,
    intersectionWithKey,
    -- * Folds
    foldl',
    foldlWithKey',
    foldr,
    foldrWithKey,
    foldMapWithKey,
    -- ** Unordered
    unorderedFoldMap,
    unorderedFoldMapWithKey,
    -- * Filter
    filter,
    filterWithKey,
    mapMaybe,
    mapMaybeWithKey,
    -- * Conversions
    keys,
    elems,
    toList,
    toRevList,
    fromList,
    toHashMap,
    fromHashMap,
    -- * Lenses
    hashMap,
    unorderedTraversal,
    -- * Debugging
    valid,
    ) where

#ifndef MIN_VERSION_aeson
#define MIN_VERSION_aeson(x,y,z) 0
#endif

import Prelude        ()
import Prelude.Compat hiding (filter, foldr, lookup, map, null)

import           Control.Applicative             (Const (..), (<**>))
import           Control.Arrow                   (first, second)
import           Data.Aeson
import qualified Data.Aeson.Encoding             as E
import           Data.Data                       (Data, Typeable)
import qualified Data.Foldable                   as F
import           Data.Functor.Apply              (Apply (..))
import           Data.Functor.Bind               (Bind (..))
import           Data.Hashable                   (Hashable (..))
import           Data.List                       (nub, sortBy)
import           Data.Maybe                      (fromMaybe)
import           Data.Ord                        (comparing)
import           Data.Semigroup                  (Semigroup (..))
import           Text.ParserCombinators.ReadPrec (prec)
import           Text.Read                       (Lexeme (..), Read (..), lexP,
                                                  parens, readListPrecDefault)

import Control.Lens                     (At (..), FoldableWithIndex (..),
                                         FunctorWithIndex (..), Index, Iso, IxValue,
                                         Ixed (..), TraversableWithIndex (..),
                                         Traversal, iso, (<&>), _1, _2)
import Control.Monad.Trans.State.Strict (State, runState, state)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

#if MIN_VERSION_base(4,7,0)
import qualified GHC.Exts as Exts
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes
#endif

-------------------------------------------------------------------------------
-- Strict Pair Int a
-------------------------------------------------------------------------------

data P a = P !Int !a
    deriving (Functor, Foldable, Traversable, Typeable, Data)

getPK :: P a -> Int
getPK (P i _) = i
{-# INLINABLE getPK #-}

getPV :: P a -> a
getPV (P _ a) = a
{-# INLINABLE getPV #-}

incPK :: Int -> P a -> P a
incPK i (P j x) = P (i + j) x
{-# INLINABLE incPK #-}

instance Eq a => Eq (P a) where
    P _ a == P _ b = a == b

instance Ord a => Ord (P a) where
    compare (P _ a) (P _ b) = compare a b

instance Show a => Show (P a) where
    showsPrec d (P _ x) = showsPrec d x

instance Hashable a => Hashable (P a) where
    hashWithSalt salt (P _ x) = hashWithSalt salt x

-------------------------------------------------------------------------------
-- InsOrdHashMap
-------------------------------------------------------------------------------

-- | 'HashMap' which tries it's best to remember insertion order of elements.

data InsOrdHashMap k v = InsOrdHashMap
    { _getIndex        :: !Int
    , getInsOrdHashMap :: !(HashMap k (P v))
    }
    deriving (Functor, Typeable, Data)

instance (Eq k, Eq v) => Eq (InsOrdHashMap k v) where
    InsOrdHashMap _ a == InsOrdHashMap _ b = a == b

instance (Ord k, Ord v) => Ord (InsOrdHashMap k v) where
    compare (InsOrdHashMap _ a) (InsOrdHashMap _ b) = compare a b

#if MIN_VERSION_base(4,9,0)
liftP :: (t1 -> t2 -> t3) -> P t1 -> P t2 -> t3
liftP f pa pb = f (getPV pa) (getPV pb)

instance Eq k => Eq1 (InsOrdHashMap k) where
    liftEq f (InsOrdHashMap _ a) (InsOrdHashMap _ b) = liftEq (liftP f) a b

instance Eq2 InsOrdHashMap where
    liftEq2 f g (InsOrdHashMap _ a) (InsOrdHashMap _ b) =
      liftEq2 f (liftP g) a b

instance Ord k => Ord1 (InsOrdHashMap k) where
    liftCompare f (InsOrdHashMap _ a) (InsOrdHashMap _ b) =
      liftCompare (liftP f) a b

instance Ord2 InsOrdHashMap where
    liftCompare2 f g (InsOrdHashMap _ a) (InsOrdHashMap _ b) =
      liftCompare2 f (liftP g) a b

instance Show2 InsOrdHashMap where
    liftShowsPrec2 spk slk spv slv d m =
        showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)
      where
        sp = liftShowsPrec2 spk slk spv slv
        sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (InsOrdHashMap k) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList
#endif

instance (Show k, Show v) => Show (InsOrdHashMap k v) where
    showsPrec d m = showParen (d > 10) $
        showString "fromList " . showsPrec 11 (toList m)

instance (Eq k, Hashable k, Read k, Read v) => Read (InsOrdHashMap k v) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs <- readPrec
      return (fromList xs)

    readListPrec = readListPrecDefault

instance (Eq k, Hashable k) => Semigroup (InsOrdHashMap k v) where
    (<>) = union

instance (Eq k, Hashable k) => Monoid (InsOrdHashMap k v) where
    mempty = empty
    mappend = union

-- We cannot derive this, as we want to ordered folding and traversing
instance Foldable (InsOrdHashMap k) where
    -- in newer base only
    -- length = length . getInsOrdHashMap
    foldMap f = foldMap (f . snd) . toList

#if MIN_VERSION_base(4,8,0)
    null = null
    toList = elems
    length = size
#endif

instance Traversable (InsOrdHashMap k) where
    traverse f m = traverseWithKey (\_ -> f) m

instance (Eq k, Hashable k) => Apply (InsOrdHashMap k) where
    (<.>) = intersectionWith id
    (<. ) = intersectionWith const
    ( .>) = intersectionWith (const id)

instance (Eq k, Hashable k) => Bind (InsOrdHashMap k) where
    m >>- f = mapMaybeWithKey (\k -> lookup k . f) m

-- | @hashWithSalt salt . toHashMap = hashWithSalt salt@.
instance (Hashable k, Hashable v) => Hashable (InsOrdHashMap k v) where
    hashWithSalt salt (InsOrdHashMap _ m) =
        hashWithSalt salt m

#if MIN_VERSION_base(4,7,0)
instance (Eq k, Hashable k) => Exts.IsList (InsOrdHashMap k v) where
    type Item (InsOrdHashMap k v) = (k, v)
    fromList = fromList
    toList   = toList
#endif

-------------------------------------------------------------------------------
-- Aeson
-------------------------------------------------------------------------------

instance (ToJSONKey k) => ToJSON1 (InsOrdHashMap k) where
    liftToJSON t _ = case toJSONKey :: ToJSONKeyFunction k of
      ToJSONKeyText f _ -> object . fmap (\(k, v) -> (f k, t v)) . toList
      ToJSONKeyValue f _ -> toJSON . fmap (\(k,v) -> toJSON (f k, t v)) . toList

    liftToEncoding t _ = case toJSONKey :: ToJSONKeyFunction k of
      ToJSONKeyText _ f ->  E.dict f t foldrWithKey
      ToJSONKeyValue _ f -> E.list (liftToEncoding2 f (E.list f) t (E.list t)) . toList

instance (ToJSONKey k, ToJSON v) => ToJSON (InsOrdHashMap k v) where
    toJSON = toJSON1
    toEncoding = toEncoding1

-------------------------------------------------------------------------------

instance (Eq k, Hashable k, FromJSONKey k) => FromJSON1 (InsOrdHashMap k) where
    liftParseJSON p pl v = fromList . HashMap.toList <$> liftParseJSON p pl v

instance (Eq k, Hashable k, FromJSONKey k, FromJSON v) => FromJSON (InsOrdHashMap k v) where
    parseJSON = parseJSON1

-------------------------------------------------------------------------------
-- Lens
-------------------------------------------------------------------------------

type instance Index (InsOrdHashMap k v) = k
type instance IxValue (InsOrdHashMap k v) = v

instance (Eq k, Hashable k) => Ixed (InsOrdHashMap k v) where
    ix k f m = case lookup k m of
         Just v  -> f v <&> \v' -> insert k v' m
         Nothing -> pure m
    {-# INLINABLE ix #-}

instance (Eq k, Hashable k) => At (InsOrdHashMap k a) where
    at k f m = f mv <&> \r -> case r of
        Nothing -> maybe m (const (delete k m)) mv
        Just v' -> insert k v' m
      where mv = lookup k m
    {-# INLINABLE at #-}

instance (Eq k, Hashable k) => FunctorWithIndex k (InsOrdHashMap k) where
    imap = mapWithKey
instance (Eq k, Hashable k) => FoldableWithIndex k (InsOrdHashMap k) where
    ifoldMap = foldMapWithKey
instance (Eq k, Hashable k) => TraversableWithIndex k (InsOrdHashMap k) where
    itraverse = traverseWithKey

-- | This is a slight lie, as roundtrip doesn't preserve ordering.
hashMap :: Iso (InsOrdHashMap k a) (InsOrdHashMap k b) (HashMap k a) (HashMap k b)
hashMap = iso toHashMap fromHashMap

unorderedTraversal :: Traversal (InsOrdHashMap k a) (InsOrdHashMap k b) a b
unorderedTraversal = hashMap . traverse

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

empty :: InsOrdHashMap k v
empty = InsOrdHashMap 0 HashMap.empty
{-# INLINABLE empty #-}

singleton :: Hashable k => k -> v -> InsOrdHashMap k v
singleton k v = InsOrdHashMap 1 (HashMap.singleton k (P 0 v))
{-# INLINABLE singleton #-}

-------------------------------------------------------------------------------
-- Basic interface
-------------------------------------------------------------------------------

null :: InsOrdHashMap k v -> Bool
null = HashMap.null . getInsOrdHashMap
{-# INLINABLE null #-}

size :: InsOrdHashMap k v -> Int
size = HashMap.size . getInsOrdHashMap
{-# INLINABLE size #-}

member :: (Eq k, Hashable k) => k -> InsOrdHashMap k a -> Bool
member k = HashMap.member k . getInsOrdHashMap
{-# INLINABLE member #-}

lookup :: (Eq k, Hashable k) => k -> InsOrdHashMap k v -> Maybe v
lookup k = fmap getPV . HashMap.lookup k . getInsOrdHashMap
{-# INLINABLE lookup #-}

lookupDefault
    :: (Eq k, Hashable k)
    => v  -- ^ Default value to return.
    -> k -> InsOrdHashMap k v -> v
lookupDefault def k m = fromMaybe def $ lookup k m
{-# INLINABLE lookupDefault #-}

delete :: (Eq k, Hashable k) => k -> InsOrdHashMap k v -> InsOrdHashMap k v
delete k (InsOrdHashMap i m) = InsOrdHashMap i $ HashMap.delete k m
{-# INLINABLE delete #-}

insert :: (Eq k, Hashable k) => k -> v -> InsOrdHashMap k v -> InsOrdHashMap k v
insert = insertWith const
{-# INLINABLE insert #-}

insertWith
    :: (Eq k, Hashable k)
    => (v -> v -> v) -> k -> v -> InsOrdHashMap k v -> InsOrdHashMap k v
insertWith f k v = alter (Just . maybe v (f v)) k
{-# INLINABLE insertWith #-}

adjust
    :: (Eq k, Hashable k)
    => (v -> v) -> k -> InsOrdHashMap k v -> InsOrdHashMap k v
adjust f = alter (fmap f)
{-# INLINABLE adjust #-}

update
    :: (Eq k, Hashable k)
    => (a -> Maybe a) -> k -> InsOrdHashMap k a -> InsOrdHashMap k a
update f = alter (>>= f)
{-# INLINABLE update #-}

alter
    :: (Eq k, Hashable k)
    => (Maybe v -> Maybe v) -> k -> InsOrdHashMap k v -> InsOrdHashMap k v
alter f k insm@(InsOrdHashMap j m) =
    case HashMap.lookup k m of
        Nothing       -> case f Nothing of
            Nothing   -> insm
            Just v    -> InsOrdHashMap (j + 1) (HashMap.insert k (P j v) m)
        Just (P i v)  -> case f (Just v) of
            Nothing   -> InsOrdHashMap j (HashMap.delete k m)
            Just u    -> InsOrdHashMap j (HashMap.insert k (P i u) m)
{-# INLINABLE alter #-}

-------------------------------------------------------------------------------
-- Combine
-------------------------------------------------------------------------------

-- | The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the result.
--
-- Ordered traversal will go thru keys in the first map first.
unionWith
    :: (Eq k, Hashable k)
    => (v -> v -> v)
    -> InsOrdHashMap k v -> InsOrdHashMap k v -> InsOrdHashMap k v
unionWith f (InsOrdHashMap i a) (InsOrdHashMap j b) =
    mk $ HashMap.unionWith f' a b'
  where
    -- the threshold is arbitrary, it meant to amortise need for packing of indices
    mk | i > 0xfffff || j >= 0xfffff = fromHashMapP
       | otherwise                   = InsOrdHashMap (i + j)
    b' = fmap (incPK i) b
    f' (P ii x) (P _ y) = P ii (f x y)

unionWithKey
    :: (Eq k, Hashable k)
    => (k -> v -> v -> v)
    -> InsOrdHashMap k v -> InsOrdHashMap k v -> InsOrdHashMap k v
unionWithKey f (InsOrdHashMap i a) (InsOrdHashMap j b) =
    InsOrdHashMap (i + j) $ HashMap.unionWithKey f' a b'
  where
    b' = fmap (incPK i) b
    f' k (P ii x) (P _ y) = P ii (f k x y)

union
    :: (Eq k, Hashable k)
    => InsOrdHashMap k v -> InsOrdHashMap k v -> InsOrdHashMap k v
union = unionWith const

unions
    :: (Eq k, Hashable k, Foldable f)
    => f (InsOrdHashMap k v) -> InsOrdHashMap k v
unions = F.foldl' union empty

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

-- | Order preserving mapping of keys.
mapKeys :: (Eq k', Hashable k') => (k -> k') -> InsOrdHashMap k v -> InsOrdHashMap k' v
mapKeys f (InsOrdHashMap i m) = InsOrdHashMap i $
    HashMap.fromList . fmap (first f) . HashMap.toList $ m

traverseKeys
    :: (Eq k', Hashable k', Applicative f)
    => (k -> f k') -> InsOrdHashMap k v -> f (InsOrdHashMap k' v)
traverseKeys f (InsOrdHashMap i m) = InsOrdHashMap i . HashMap.fromList <$>
    (traverse . _1) f (HashMap.toList m)

map :: (v1 -> v2) -> InsOrdHashMap k v1 -> InsOrdHashMap k v2
map = fmap

mapWithKey :: (k -> v1 -> v2) -> InsOrdHashMap k v1 -> InsOrdHashMap k v2
mapWithKey f (InsOrdHashMap i m) =
    InsOrdHashMap i $ HashMap.mapWithKey f' m
  where
    f' k (P j x) = P j (f k x)

foldMapWithKey :: Monoid m => (k -> a -> m) -> InsOrdHashMap k a -> m
foldMapWithKey f = foldMap (uncurry f) . toList

traverseWithKey :: Applicative f => (k -> a -> f b) -> InsOrdHashMap k a -> f (InsOrdHashMap k b)
traverseWithKey f (InsOrdHashMap n m) = InsOrdHashMap n <$> retractSortedAp
    (HashMap.traverseWithKey (\k (P i v) -> liftSortedAp i (P i <$> f k v)) m)

-- Sort using insertion sort
-- Hopefully it's fast enough for where we need it
-- otherwise: https://gist.github.com/treeowl/9621f58d55fe0c4f9162be0e074b1b29
-- http://elvishjerricco.github.io/2017/03/23/applicative-sorting.html also related

-- Free applicative which re-orders effects
-- Mostly from Edward Kmett's `free` package.
data SortedAp f a where
    Pure :: a -> SortedAp f a
    SortedAp   :: !Int -> f a -> SortedAp f (a -> b) -> SortedAp f b

instance Functor (SortedAp f) where
    fmap f (Pure a)   = Pure (f a)
    fmap f (SortedAp i x y)   = SortedAp i x ((f .) <$> y)

instance Applicative (SortedAp f) where
    pure = Pure
    Pure f <*> y = fmap f y
    -- This is different from real Ap
    f <*> Pure y = fmap ($ y) f
    f@(SortedAp i x y) <*> z@(SortedAp j u v)
        | i < j     = SortedAp i x (flip <$> y <*> z)
        | otherwise = SortedAp j u ((.) <$> f <*> v)

liftSortedAp :: Int -> f a -> SortedAp f a
liftSortedAp i x = SortedAp i x (Pure id)

retractSortedAp :: Applicative f => SortedAp f a -> f a
retractSortedAp (Pure x) = pure x
retractSortedAp (SortedAp _ f x) = f <**> retractSortedAp x

-------------------------------------------------------------------------------
-- Unordered
-------------------------------------------------------------------------------

-- | More efficient than 'foldMap', when folding in insertion order is not important.
unorderedFoldMap :: Monoid m => (a -> m) -> InsOrdHashMap k a -> m
unorderedFoldMap f (InsOrdHashMap _ m) = foldMap (f . getPV) m

-- | More efficient than 'foldMapWithKey', when folding in insertion order is not important.
unorderedFoldMapWithKey :: Monoid m => (k -> a -> m) -> InsOrdHashMap k a -> m
unorderedFoldMapWithKey f m =
    getConst (unorderedTraverseWithKey (\k a -> Const (f k a)) m)

-- | More efficient than 'traverse', when traversing in insertion order is not important.
unorderedTraverse :: Applicative f => (a -> f b) -> InsOrdHashMap k a -> f (InsOrdHashMap k b)
unorderedTraverse f (InsOrdHashMap i m) =
    InsOrdHashMap i <$> (traverse . traverse) f m

-- | More efficient than `traverseWithKey`, when traversing in insertion order is not important.
unorderedTraverseWithKey :: Applicative f => (k -> a -> f b) -> InsOrdHashMap k a -> f (InsOrdHashMap k b)
unorderedTraverseWithKey f (InsOrdHashMap i m) =
    InsOrdHashMap i <$> HashMap.traverseWithKey f' m
  where
    f' k (P j x) = P j <$> f k x

-------------------------------------------------------------------------------
-- Difference and intersection
-------------------------------------------------------------------------------

difference
    :: (Eq k, Hashable k)
    => InsOrdHashMap k v -> InsOrdHashMap k w -> InsOrdHashMap k v
difference (InsOrdHashMap i a) (InsOrdHashMap _ b) =
    InsOrdHashMap i $ HashMap.difference a b

intersection
    :: (Eq k, Hashable k)
    => InsOrdHashMap k v -> InsOrdHashMap k w -> InsOrdHashMap k v
intersection = intersectionWith const

intersectionWith
    :: (Eq k, Hashable k)
    => (v1 -> v2 -> v3)
    -> InsOrdHashMap k v1 -> InsOrdHashMap k v2 -> InsOrdHashMap k v3
intersectionWith f = intersectionWithKey (\_ -> f)

intersectionWithKey
    :: (Eq k, Hashable k)
    => (k -> v1 -> v2 -> v3)
    -> InsOrdHashMap k v1 -> InsOrdHashMap k v2 -> InsOrdHashMap k v3
intersectionWithKey f (InsOrdHashMap i a) (InsOrdHashMap _ b) =
    InsOrdHashMap i $ HashMap.intersectionWithKey f' a b
  where
    f' k (P j x) (P _ y) = P j (f k x y)

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

foldl' :: (a -> v -> a) -> a -> InsOrdHashMap k v -> a
foldl' f x = F.foldl' f' x . toList
  where
    f' a (_, v) = f a v

foldlWithKey' :: (a -> k -> v -> a) -> a -> InsOrdHashMap k v -> a
foldlWithKey' f x = F.foldl' f' x . toList
  where
    f' a (k, v) = f a k v

foldr :: (v -> a -> a) -> a -> InsOrdHashMap k v -> a
foldr f x = F.foldr f' x . toList
  where
    f' (_, v) a = f v a

foldrWithKey :: (k -> v -> a -> a) -> a -> InsOrdHashMap k v -> a
foldrWithKey f x = F.foldr f' x . toList
  where
    f' (k, v) a = f k v a

-------------------------------------------------------------------------------
-- Filter
-------------------------------------------------------------------------------

filter :: (v -> Bool) -> InsOrdHashMap k v -> InsOrdHashMap k v
filter f (InsOrdHashMap i m) =
    InsOrdHashMap i $ HashMap.filter (f . getPV) m

filterWithKey :: (k -> v -> Bool) -> InsOrdHashMap k v -> InsOrdHashMap k v
filterWithKey f (InsOrdHashMap i m) =
    InsOrdHashMap i $ HashMap.filterWithKey f' m
  where
    f' k (P _ x) = f k x

mapMaybe :: (v1 -> Maybe v2) -> InsOrdHashMap k v1 -> InsOrdHashMap k v2
mapMaybe f (InsOrdHashMap i m) = InsOrdHashMap i $ HashMap.mapMaybe f' m
  where
    f' (P j x) = P j <$> f x

mapMaybeWithKey :: (k -> v1 -> Maybe v2) -> InsOrdHashMap k v1 -> InsOrdHashMap k v2
mapMaybeWithKey f (InsOrdHashMap i m) =
    InsOrdHashMap i $ HashMap.mapMaybeWithKey f' m
  where
    f' k (P j x) = P j <$> f k x

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

keys :: InsOrdHashMap k v -> [k]
keys = fmap fst . toList
{-# INLINABLE keys #-}

elems :: InsOrdHashMap k v -> [v]
elems = fmap snd . toList
{-# INLINABLE elems #-}

fromList :: forall k v. (Eq k, Hashable k) => [(k, v)] -> InsOrdHashMap k v
fromList
    = mk
    . flip runState 0
    . (traverse . _2) newP
  where
    mk :: ([(k, P v)], Int) -> InsOrdHashMap k v
    mk (m, i) = InsOrdHashMap i (HashMap.fromList m)

toList :: InsOrdHashMap k v -> [(k, v)]
toList
    = fmap (second getPV)
    . sortBy (comparing (getPK . snd))
    . HashMap.toList
    . getInsOrdHashMap

toRevList :: InsOrdHashMap k v -> [(k, v)]
toRevList
    = fmap (second getPV)
    . sortBy (flip $ comparing (getPK . snd))
    . HashMap.toList
    . getInsOrdHashMap

fromHashMap :: HashMap k v -> InsOrdHashMap k v
fromHashMap = mk . flip runState 0 . traverse newP
  where
    mk (m, i) = InsOrdHashMap i m

toHashMap :: InsOrdHashMap k v -> HashMap k v
toHashMap (InsOrdHashMap _ m) = fmap getPV m

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

-- TODO: more efficient way is to do two traversals
-- - collect the indexes
-- - pack the indexes (Map old new)
-- - traverse second time, changing the indexes
fromHashMapP :: HashMap k (P v) -> InsOrdHashMap k v
fromHashMapP = mk . flip runState 0 . retractSortedAp . traverse f
  where
    mk (m, i) = InsOrdHashMap i m
    f (P i v) = liftSortedAp i (newP v)

-- | Test if the internal map structure is valid.
valid :: InsOrdHashMap k v -> Bool
valid (InsOrdHashMap i m) = indexesDistinct && indexesSmaller
  where
    indexes :: [Int]
    indexes = getPK <$> HashMap.elems m

    indexesDistinct = indexes == nub indexes
    indexesSmaller  = all (< i) indexes

newP :: a -> State Int (P a)
newP x = state $ \s -> (P s x, s + 1)
