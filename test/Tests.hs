module Main (main) where

import Prelude        ()
import Prelude.Compat

import Control.Lens   (folded, ifolded, (^..), (^@..))
import Data.Function  (on)
import Data.Hashable  (Hashable (..))
import Data.List      (nubBy)
import Data.Semigroup ((<>))
import Data.Traversable (foldMapDefault)
import Data.Word      (Word8)
import Text.Read      (readMaybe)

import qualified Data.Aeson                 as Aeson
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.HashMap.Strict.InsOrd as InsOrd

import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ testGroup "Properties" $
        [ testProperty "toList . fromList ~= id" $ toListFromList
        , testProperty "toList distributes over mappend" $ toListMappendDistribute
        , testProperty "behaves like HashMap" $ operationModel
        , testProperty "valid" $ validProperty
        , testProperty "Hashable agree" $ hashableProperty
        , testProperty "aeson roundtrip" $ aesonRoundtrip
        , testProperty "show . read = id" showReadRoundtrip
        ]
    , testGroup "Regressions"
        [ testProperty "issue 12 Foldable" $ issue12a
        , testProperty "issue 12 Traversable" $ issue12b
        , testProperty "issue 12 FoldableWithIndex ^.." $ issue12c
        , testProperty "issue 12 FoldableWithIndex ^@.." $ issue12d
        ]
    ]

toListFromList :: [(Int, Int)] -> Property
toListFromList l = l' === InsOrd.toList (InsOrd.fromList l)
  where l' = reverse . nubBy (on (==) fst) .  reverse $ l

toListMappendDistribute :: [(Int, Int)] -> [(Int, Int)] -> Property
toListMappendDistribute a b = rhs === lhs
  where
    a' = InsOrd.fromList a
    b' = foldr InsOrd.delete (InsOrd.fromList b) (InsOrd.keys a')
    rhs = InsOrd.toList (a' <> b')
    lhs = InsOrd.toList a' <> InsOrd.toList b'

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data Operation k v
    = FromList [(k, v)]
    | Empty
    | Singleton k v
    | Insert k v (Operation k v)
    | Delete k (Operation k v)
    | Union (Operation k v) (Operation k v)
    | Difference (Operation k v) (Operation k v)
    | Intersection (Operation k v) (Operation k v)
    | Filter (Fun v Bool) (Operation k v)
    deriving (Show)

instance (Arbitrary k, Arbitrary v, Function v, CoArbitrary v) => Arbitrary (Operation k v) where
    arbitrary = sized a
      where
          term =
              [ FromList <$> arbitrary
              , pure Empty
              , Singleton <$> arbitrary <*> arbitrary
              ]
          a 0 = oneof term
          a n = oneof $ term ++
              [ Insert <$> arbitrary <*> arbitrary <*> aMinus1
              , Delete <$> arbitrary <*> aMinus1
              , Union <$> aDiv2 <*> aDiv2
              , Difference <$> aDiv2 <*> aDiv2
              , Intersection <$> aDiv2 <*> aDiv2
              , Filter <$> arbitrary <*> aMinus1
              ]
            where
              aMinus1 = a (n - 1)
              aDiv2   = a (n `div` 2)

evalOpInsOrd
    :: (Eq k, Hashable k)
    => Operation k v -> InsOrd.InsOrdHashMap k v
evalOpInsOrd op = case op of
    FromList l         -> InsOrd.fromList l
    Empty              -> InsOrd.empty
    Singleton k v      -> InsOrd.singleton k v
    Insert k v a       -> InsOrd.insert k v (evalOpInsOrd a)
    Delete k a         -> InsOrd.delete k (evalOpInsOrd a)
    Union a b          -> InsOrd.union (evalOpInsOrd a) (evalOpInsOrd b)
    Difference a b     -> InsOrd.difference (evalOpInsOrd a) (evalOpInsOrd b)
    Intersection a b   -> InsOrd.intersection (evalOpInsOrd a) (evalOpInsOrd b)
    Filter (Fun _ f) a -> InsOrd.filter f (evalOpInsOrd a)

evalOpHashMap
    :: (Eq k, Hashable k)
    => Operation k v-> HashMap.HashMap k v
evalOpHashMap op = case op of
    FromList l         -> HashMap.fromList l
    Empty              -> HashMap.empty
    Singleton k v      -> HashMap.singleton k v
    Insert k v a       -> HashMap.insert k v (evalOpHashMap a)
    Delete k a         -> HashMap.delete k (evalOpHashMap a)
    Union a b          -> HashMap.union (evalOpHashMap a) (evalOpHashMap b)
    Difference a b     -> HashMap.difference (evalOpHashMap a) (evalOpHashMap b)
    Intersection a b   -> HashMap.intersection (evalOpHashMap a) (evalOpHashMap b)
    Filter (Fun _ f) a -> HashMap.filter f (evalOpHashMap a)

operationModel :: Operation Word8 Int -> Property
operationModel op = rhs === lhs
  where
    iom = evalOpInsOrd op
    lhs = InsOrd.toHashMap iom
    rhs = evalOpHashMap op

validProperty :: Operation Word8 Int -> Property
validProperty op = property $ InsOrd.valid iom
  where
    iom = evalOpInsOrd op

hashableProperty :: Operation Word8 Int -> Int -> Property
hashableProperty op salt = rhs === lhs
  where
    iom = evalOpInsOrd op
    lhs = hashWithSalt salt $ iom
    rhs = hashWithSalt salt $ evalOpHashMap op

aesonRoundtrip :: Operation Int Int -> Property
aesonRoundtrip op = rhs === lhs
  where
    iom = evalOpInsOrd op
    rhs = Right iom 
    lhs = Aeson.eitherDecode $ Aeson.encode iom

showReadRoundtrip :: Operation Word8 Int -> Property
showReadRoundtrip op = rhs === lhs
  where
    iom = evalOpInsOrd op
    rhs = Just iom
    lhs = readMaybe $ show iom

-------------------------------------------------------------------------------
-- Regressions
-------------------------------------------------------------------------------

issue12a :: Property
issue12a = (m ^.. folded) === "wold"
  where
    m :: InsOrd.InsOrdHashMap Char Char
    m = InsOrd.fromList  (zip "hello" "world")

issue12b :: Property
issue12b = foldMapDefault (:[]) m === "wold"
  where
    m :: InsOrd.InsOrdHashMap Char Char
    m = InsOrd.fromList  (zip "hello" "world")

issue12c :: Property
issue12c = (m ^.. ifolded) === "wold"
  where
    m :: InsOrd.InsOrdHashMap Char Char
    m = InsOrd.fromList  (zip "hello" "world")

issue12d :: Property
issue12d = (m ^@.. ifolded) === (zip "helo" "wold")
  where
    m :: InsOrd.InsOrdHashMap Char Char
    m = InsOrd.fromList  (zip "hello" "world")
