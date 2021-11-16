{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
-- | 'InsOrdHashSet' is like 'HashMap', but it folds in insertion order.
--
-- This module interface mimics "Data.HashSet", with some additions.
module Data.HashSet.InsOrd (
    InsOrdHashSet,
    -- * Construction
    empty,
    singleton,
    -- * Basic interface
    null,
    size,
    member,
    insert,
    delete,
    -- * Combine
    union,
    -- * Transformations
    map,
    -- ** Unordered
    -- * Difference and intersection
    difference,
    intersection,
    -- * Folds
    -- ** Unordered
    -- * Filter
    filter,
    -- * Conversions
    toList,
    fromList,
    toHashSet,
    fromHashSet,
    -- * Lenses
    hashSet,
    -- * Debugging
    valid,
    )where

import Prelude hiding (filter, foldr, lookup, map, null)

import Control.Arrow                   (first)
import Control.DeepSeq                 (NFData (..))
import Data.Aeson
import Data.Data                       (Data, Typeable)
import Data.Hashable                   (Hashable (..))
import Data.List                       (nub, sortBy)
import Data.Ord                        (comparing)
import Data.Semigroup                  (Semigroup (..))
import Text.ParserCombinators.ReadPrec (prec)
import Text.Read
       (Lexeme (..), Read (..), lexP, parens, readListPrecDefault)

import Control.Lens
       (At (..), Contains (..), Index, Iso', IxValue, Ixed (..), iso, (<&>))
import Control.Monad.Trans.State.Strict (State, runState, state)

import qualified Control.Lens as Lens
import qualified Optics.At    as Optics
import qualified Optics.Core  as Optics

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet

import qualified Data.Foldable
import qualified GHC.Exts      as Exts

#if MIN_VERSION_deepseq(1,4,3)
import qualified Control.DeepSeq as NF
#endif

import Data.HashMap.InsOrd.Internal

-------------------------------------------------------------------------------
-- InsOrdHashSet
-------------------------------------------------------------------------------

-- | 'HashSet' which tries its best to remember insertion order of elements.

data InsOrdHashSet k = InsOrdHashSet
    { _getIndex        :: !Int
    , getInsOrdHashSet :: !(HashMap k Int)
    }
    deriving (Typeable, Data)

-- | @since 0.2.5
instance NFData k => NFData (InsOrdHashSet k) where
    rnf (InsOrdHashSet _ hs) = rnf hs

#if MIN_VERSION_deepseq(1,4,3)
-- | @since 0.2.5
instance NF.NFData1 InsOrdHashSet where
    liftRnf rnf1 (InsOrdHashSet _ m) = NF.liftRnf2 rnf1 rnf m
#endif

instance Eq k => Eq (InsOrdHashSet k) where
    InsOrdHashSet _ a == InsOrdHashSet _ b = a == b

instance Show k => Show (InsOrdHashSet k) where
    showsPrec d m = showParen (d > 10) $
        showString "fromList " . showsPrec 11 (toList m)

instance (Eq k, Hashable k, Read k) => Read (InsOrdHashSet k) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs <- readPrec
      return (fromList xs)

    readListPrec = readListPrecDefault

instance (Eq k, Hashable k) => Semigroup (InsOrdHashSet k) where
    (<>) = union

instance (Eq k, Hashable k) => Monoid (InsOrdHashSet k) where
    mempty = empty
    mappend = union

instance Foldable InsOrdHashSet where
    -- in newer base only
    -- length = length . getInsOrdHashSet
    foldMap f = foldMap f . toList

    null = null
    toList = toList
    length = size

-- | @'hashWithSalt' salt . 'toHashSet' = 'hashWithSalt' salt@.
instance Hashable k => Hashable (InsOrdHashSet k) where
    hashWithSalt salt (InsOrdHashSet _ m) =
        hashWithSalt salt m

instance (Eq k, Hashable k) => Exts.IsList (InsOrdHashSet k) where
    type Item (InsOrdHashSet k) = k
    fromList = fromList
    toList   = toList

-------------------------------------------------------------------------------
-- Aeson
-------------------------------------------------------------------------------

instance ToJSON a => ToJSON (InsOrdHashSet a) where
    toJSON     = toJSON . toList
    toEncoding = toEncoding . toList

instance (Eq a, Hashable a, FromJSON a) => FromJSON (InsOrdHashSet a) where
    parseJSON v = fromList <$> parseJSON v

-------------------------------------------------------------------------------
-- Lens
-------------------------------------------------------------------------------

type instance Index (InsOrdHashSet a) = a
type instance IxValue (InsOrdHashSet a) = ()

instance (Eq k, Hashable k) => Ixed (InsOrdHashSet k) where
    ix k f (InsOrdHashSet i m) = InsOrdHashSet i <$> ix k (\j -> j <$ f ()) m
    {-# INLINE ix #-}

instance (Eq k, Hashable k) => At (InsOrdHashSet k) where
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (delete k m)) mv
    Just () -> insert k m
    where mv = if member k m then Just () else Nothing
  {-# INLINE at #-}

instance (Eq a, Hashable a) => Contains (InsOrdHashSet a) where
  contains k f s = f (member k s) <&> \b ->
    if b then insert k s else delete k s
  {-# INLINE contains #-}

-- | This is a slight lie, as roundtrip doesn't preserve ordering.
hashSet :: Iso' (InsOrdHashSet a) (HashSet a)
hashSet = iso toHashSet fromHashSet

-------------------------------------------------------------------------------
-- Optics
-------------------------------------------------------------------------------

type instance Optics.Index (InsOrdHashSet a) = a
type instance Optics.IxValue (InsOrdHashSet a) = ()

instance (Eq k, Hashable k) => Optics.Ixed (InsOrdHashSet k) where
    ix k = Optics.atraversalVL $ \point f (InsOrdHashSet i m) ->
      InsOrdHashSet i <$>
#if MIN_VERSION_optics_core(0,3,0)
          Optics.atraverseOf
#else
          Optics.toAtraversalVL
#endif
          (Optics.ix k) point (\j -> j <$ f ()) m
    {-# INLINE ix #-}

instance (Eq k, Hashable k) => Optics.At (InsOrdHashSet k) where
    at k = Optics.lensVL $ \f m -> Lens.at k f m
    {-# INLINE at #-}

instance (Eq a, Hashable a) => Optics.Contains (InsOrdHashSet a) where
    contains k = Optics.lensVL $ \f s -> Lens.contains k f s
    {-# INLINE contains #-}

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

empty :: InsOrdHashSet k
empty = InsOrdHashSet 0 HashMap.empty
{-# INLINABLE empty #-}

singleton :: Hashable k => k -> InsOrdHashSet k
singleton k = InsOrdHashSet 1 (HashMap.singleton k 0)
{-# INLINABLE singleton #-}

-------------------------------------------------------------------------------
-- Basic interface
-------------------------------------------------------------------------------

null :: InsOrdHashSet k -> Bool
null = HashMap.null . getInsOrdHashSet
{-# INLINABLE null #-}

size :: InsOrdHashSet k -> Int
size = HashMap.size . getInsOrdHashSet
{-# INLINABLE size #-}

member :: (Eq k, Hashable k) => k -> InsOrdHashSet k -> Bool
member k = HashMap.member k . getInsOrdHashSet
{-# INLINABLE member #-}

insert :: (Eq k, Hashable k) => k -> InsOrdHashSet k -> InsOrdHashSet k
insert k (InsOrdHashSet i m) = InsOrdHashSet (i + 1) (HashMap.insert k i m)

delete :: (Eq k, Hashable k) => k -> InsOrdHashSet k -> InsOrdHashSet k
delete k (InsOrdHashSet i m) = InsOrdHashSet i (HashMap.delete k m)

-------------------------------------------------------------------------------
-- Combine
-------------------------------------------------------------------------------

union
    :: (Eq k, Hashable k)
    => InsOrdHashSet k -> InsOrdHashSet k -> InsOrdHashSet k
union (InsOrdHashSet i a) (InsOrdHashSet j b) =
    mk $ HashMap.union a b'
  where
    mk | i >= 0xfffff || j >= 0xfffff = fromHashMapInt
       | otherwise                    = InsOrdHashSet (i + j)

    b' = fmap (\k -> k + i + 1) b

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

map :: (Hashable b, Eq b) => (a -> b) -> InsOrdHashSet a -> InsOrdHashSet b
map f (InsOrdHashSet i m) = InsOrdHashSet i
    $ HashMap.fromList . fmap (first f) . HashMap.toList
    $ m

-------------------------------------------------------------------------------
-- Difference and intersection
-------------------------------------------------------------------------------

difference :: (Eq a, Hashable a) => InsOrdHashSet a -> InsOrdHashSet a -> InsOrdHashSet a
difference (InsOrdHashSet i a) (InsOrdHashSet _ b) =
    InsOrdHashSet i $ HashMap.difference a b

intersection :: (Eq a, Hashable a) => InsOrdHashSet a -> InsOrdHashSet a -> InsOrdHashSet a
intersection (InsOrdHashSet i a) (InsOrdHashSet _ b) =
    InsOrdHashSet i $ HashMap.intersection a b

-------------------------------------------------------------------------------
-- Filter
-------------------------------------------------------------------------------

filter :: (a -> Bool) -> InsOrdHashSet a -> InsOrdHashSet a
filter p (InsOrdHashSet i m) = InsOrdHashSet i $
    HashMap.filterWithKey (\k _ -> p k) m

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

fromList :: (Eq k, Hashable k) => [k] -> InsOrdHashSet k
fromList = mk . flip runState 0 . traverse newInt where
    mk (m, i) = InsOrdHashSet i (HashMap.fromList m)

toList :: InsOrdHashSet k -> [k]
toList
    = fmap fst
    . sortBy (comparing snd)
    . HashMap.toList
    . getInsOrdHashSet

fromHashSet :: HashSet k -> InsOrdHashSet k
fromHashSet = mk . flip runState 0 . traverse (const newInt') . HashSet.toMap where
    mk (m, i) = InsOrdHashSet i m

toHashSet :: InsOrdHashSet k -> HashSet k
toHashSet (InsOrdHashSet _ m) =
#if MIN_VERSION_unordered_containers(0,2,10)
    HashMap.keysSet m
#else
    HashSet.fromMap (fmap (const ()) m)
#endif

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

fromHashMapInt :: HashMap k Int -> InsOrdHashSet k
fromHashMapInt = mk . flip runState 0 . retractSortedAp . traverse f
  where
    mk (m, i) = InsOrdHashSet i m
    f i = liftSortedAp i newInt'

newInt :: a -> State Int (a, Int)
newInt a = state $ \s -> ((a, s), s + 1)

newInt' :: State Int Int
newInt' = state $ \s -> (s, s + 1)

-------------------------------------------------------------------------------
-- Valid
-------------------------------------------------------------------------------

-- | Test if the internal map structure is valid.
valid :: InsOrdHashSet a -> Bool
valid (InsOrdHashSet i m) = indexesDistinct && indexesSmaller
  where
    indexes :: [Int]
    indexes = HashMap.elems m

    indexesDistinct = indexes == nub indexes
    indexesSmaller  = all (< i) indexes
