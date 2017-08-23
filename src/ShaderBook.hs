{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module ShaderBook where

import Hylogen.WithHylide

import Utils

output :: Program
output =
    toProgram $ circle black transparent 0.003 0.33

opaque :: Vec3 -> Vec4
opaque c =
        vec4 (x_ c, y_ c, z_ c, 1.0)

-- CH 6
hsbBars :: Vec4
hsbBars =
    opaque . hsb2rgb $ vec3 (x_ uv, 1.0, y_ uv)

hsb2rgb :: Vec3 -> Vec3
hsb2rgb c =
    let
        rgb =
            clamp 0.0 1.0 $
                (abs
                    ((xxx_ c * 6.0 + vec3 (0.0, 4.0, 2.0))
                     `mod_` 6.0)
                 - 3.0)
                - 1.0
        rgb' =
            rgb * rgb * (3.0 - 2.0 * rgb)
    in
        zzz_ c * mix (y_ c) 1.0 rgb'


hsbRadial :: Vec4
hsbRadial =
    let
        toCenter = 0.5 - uv
        angle = atan (y_ toCenter / x_ toCenter)
        radius = len toCenter * 2.0
    in
        opaque . hsb2rgb $ vec3 (angle / (2 * 3.14) + 0.5, radius, 1.0)


-- CH 7
borders :: Vec4
borders =
    let
        bottomBorder =
            step 0.4 uv
        topBorder =
            step 0.1 (1.0 - uv)
        colorPercent =
            x_ bottomBorder * y_ bottomBorder * x_ topBorder * y_ topBorder
    in
        opaque $ copy colorPercent

rectangle :: Vec1 -> Vec1 -> Vec4
rectangle width height =
    let
        h' =
            (1 - height ) / 2
        w' =
            (1 - width) / 2
        bottom =
            step h' $ y_ uv
        top =
            rStep (1 - h') $ y_ uv
        left =
            step w' $ x_ uv
        right =
            rStep (1 - w') $ x_ uv
    in
        opaque . copy $ bottom * top * left * right

circle :: Vec4 -> Vec4 -> Vec1 -> Vec1 -> Vec4
circle color fillColor fuzz radius =
    let
        d =
            len $ 0.5 - uv
        alpha =
            (smoothStep (radius - fuzz) radius d -
                 smoothStep radius (radius + fuzz) d)

    in
        sel (d `leq` (radius + fuzz)) (vec4 (0.0, 0.0, 0.0, alpha)) transparent
