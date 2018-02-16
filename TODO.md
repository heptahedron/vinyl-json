# TODO

- Figure out good way to convert between `Data.Vinyl.Derived.FieldRec`
  and `Data.Vinyl.Json.JsonRec`
- Add lenses/prisms for `JsonField` values?
- Remove `Show` instances to get rid of dependency on
  `Data.ByteString`?
  - Or just make them similar to the normal `Show` instances for
    `Data.Vinyl.Derived.FieldRec`
