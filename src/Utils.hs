module Utils where

import Hylogen.WithHylide


(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

v3 :: Vec1 -> Vec1 -> Vec1 -> Vec3
v3 x y z =
    vec3 (x, y, z)

v4 :: Vec1 -> Vec1 -> Vec1 -> Vec1 -> Vec4
v4 x y z w =
    vec4 (x, y, z, w)

opaque :: Vec3 -> Vec4
opaque c =
    vec4 (x_ c, y_ c, z_ c, 1.0)


-- Colors
transparent :: Vec4
transparent =
    v4 0 0 0 0

black :: Vec4
black =
    opaque $ v3 0 0 0

white :: Vec4
white =
    opaque $ v3 1 1 1

red :: Vec4
red =
    opaque $ v3 1 0 0

green :: Vec4
green =
    opaque $ v3 0 1 0

blue :: Vec4
blue =
    opaque $ v3 0 0 1



-- GLSL Funcs

-- | `1` when value > edge, else `0`
step :: (Veccable v) => Vec v -> Vec v -> Vec v
step edge val =
    sel (val `gt` edge) 1.0 0.0

-- | `1` when edge > value, else `0`
rStep :: (Veccable v) => Vec v -> Vec v -> Vec v
rStep =
    flip step

-- | Hermite interpolation between two values
smoothStep :: (Veccable v) => Vec v -> Vec v -> Vec v -> Vec v
smoothStep lo hi val =
        let
            clamped = clamp 0.0 1.0 ((val - lo) / (hi - lo))
        in
            clamped * clamped * ( 3 - 2 * clamped )



-- Layout

-- | Layer a color on top of the current one using Alpha Blending.
withLayer :: Vec4 -> Vec4 -> Vec4
withLayer nextLayer currentLayer =
    let
        alphaBlend =
            copy (w_ nextLayer) * xyz_ nextLayer
            + (copy (1 - w_ nextLayer) * xyz_ currentLayer)
    in
        vec4 ( x_ alphaBlend
             , y_ alphaBlend
             , z_ alphaBlend
             , clamp 0 1 $ w_ nextLayer + w_ currentLayer
             )

-- | Layer a list of colors on top of each other, from left to right.
layers :: [Vec4] -> Vec4
layers =
    foldl (flip withLayer) transparent
