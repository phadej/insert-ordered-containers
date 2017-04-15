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
