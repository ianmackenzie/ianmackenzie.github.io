# Transformations

A large portion of `elm-geometry` consists of functions for translating, rotating, scaling,
mirroring and otherwise transforming geometric objects. These functions work much the same way
across different modules - rotating a `Triangle2d` is really no different from rotating a `Point2d`
or an `Arc2d`. This page describes the general 2D and 3D transformations that apply to almost all
objects.

## 2D transformations

### Translation

Translation functions all have a signature similar to

```elm
Point2d.translateBy :
    Vector2d units coordinates
    -> Point2d units coordinates
    -> Point2d units coordinates
```

which applies the given translation (displacement) to the given object. For example,

```elm
point =
    Point2d.fromCoordinates
        ( Length.meters 3
        , Length.meters 4
        )

displacement =
    Vector2d.fromComponents
        ( Length.centimenters 1
        , Length.centimeters 2
        )

Point2d.translateBy displacement point
--> Point2d.fromCoordinates
-->     ( Length.meters 3.01
-->     , Length.meters 4.02
-->     )
```

### Rotation

Rotation functions have the following form:

```elm
Triangle2d.rotateAround :
    Point2d units coordinates
    -> Angle
    -> Triangle2d units coordinates
    -> Triangle2d units coordinates
```

This means "rotate _around_ the given center point, by the given angle". For example, for a given
triangle, we might choose to rotate around

- the global origin point
- the triangle's centroid (center point)
- one of the triangle's vertices
- or any other point we want!

To rotate a triangle around its own centroid by 90 degrees:

```elm
rotatedTriangle =
    triangle
        |> Triangle2d.rotateAround
            (Triangle2d.centroid triangle)
            (Angle.degrees 90)
```

### Scaling

Similar to rotation, scaling functions take as the first argument a point that will be used as the
center point for the scaling operation:

```elm
Polygon2d.scaleAbout :
    Point2d units coordinates
    -> Float
    -> Polygon2d units coordinates
    -> Polygon2d units coordinates
```

This will cause the object to expand away from (or contract towards) the given point, by the given
scale. A scale value of 1 is a no-op. Negative scale values will generally "turn the object inside
out"; `elm-geometry` tries to handle these cases correctly but doing so is confusing, error-prone
and doesn't make a lot of sense in most situations.

## 3D transformations
