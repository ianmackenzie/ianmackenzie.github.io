# Interop

Since `elm-geometry` uses the `Quantity` type from [`elm-units`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/)
to represent point coordinates and vector components, a bit of extra work is required when
working with other code that uses plain `Float` values:

- When converting *from* plain `Float` values, you must provide a unit conversion function such as
  `Length.feet` that takes a `Float` and produces a `Quantity Float units`.
- When converting *to* plain `Float` values, you must provide a unit conversion function such as
  `Length.inFeet` that takes a `Quantity Float units` and produces a `Float`.

For example, to construct a `Point2d` representing an on-screen point with coordinates in pixels,
you might use

```elm
point =
    Point2d.fromTuple Pixels.pixels ( 200, 300 )
```
