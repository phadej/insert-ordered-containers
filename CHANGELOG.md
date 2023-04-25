- 0.2.5.2
    - Actually drop `semigroups` dependency
    - Update bounds

- 0.2.5.1
    - Drop unnecessary dependencies `semigroups`, `base-compat`
    - Update bounds

- 0.2.5
    - Add `NFData(/1/2)` instances

- 0.2.4
    - Add `indexed-traversable` instances
    - lens-5 and optics-0.4 support

- 0.2.3
    - Add support for indexed `optics`
    - Only support GHC-8.0+

- 0.2.2
    - Add `Data.HashSet.InsOrd`

- 0.2.1.0
    - Fix `Traversable`, `TraversableWithIndex`, `FoldableWithIndex` to traverse
      in insertion order
      ([#12](https://github.com/phadej/insert-ordered-containers/issues/12))
    - Add `unorderedTraverse`, `unorderedTraverseWithKey`, `unoderedFoldMap`, and
      `unorderedFoldMapWithKey`.
    - `union` doesn't overflow the internal counter
      ([#10](https://github.com/phadej/insert-ordered-containers/issues/10))

- 0.2.0.0
    - Use `aeson-1`
    - removed our `FromJSONKey` and `ToJSONKey` in favour of `aeson` variants
