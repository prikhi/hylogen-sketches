{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
module PietRectangles where

-- TODO: Border widths in addition to fuzzes?

import Hylogen.WithHylide

import Shapes
import Utils


offwhite :: Vec4
offwhite =
    vec4 (249 / 256.0, 253 / 256.0, 245 / 256.0, 0.98)

output :: Program
output =
    let
        xPixel =
            1.0 / x_ resolution
        yPixel =
            1.0 / y_ resolution
        avgPixel =
            sum [xPixel, yPixel] / 2

        borderWidth =
            3 * avgPixel

        pietRectangle fillColor position width height =
            Rectangle
                { color = black
                , fillColor
                , position
                , fuzz = borderWidth
                , width
                , height
                }

        program =
            layers $ offwhite : map draw
                [ pietRectangle
                    (opaque $ v3 1 1 0)
                    (vec2 (0.665, 0.95))
                    0.33
                    0.25
                , pietRectangle
                    red
                    (vec2 (0.1, 0.3))
                        0.37
                        0.2
                , pietRectangle
                    transparent
                    (vec2 (0.2 - 2 * avgPixel, 0.1))
                        0.4
                        0.2
                , pietRectangle
                    transparent
                    (vec2 (0.3, 0.65))
                    0.4
                    0.5
                , pietRectangle
                    white
                    (vec2 (0.19, 0.65))
                    0.3
                    0.5
                , pietRectangle
                    transparent
                    (vec2 (0.75, 0.3))
                    0.5
                    0.7
                , pietRectangle
                    blue
                    (vec2 (0.449, 0.15))
                    0.101
                    0.1
                , pietRectangle
                    white
                    (vec2 (0.875, 0.2))
                    0.30
                    0.60
                , pietRectangle
                    black
                    (vec2 (0.665, 0.5))
                    0.33
                    0.15
                ]
    in
        toProgram program
