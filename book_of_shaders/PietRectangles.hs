{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module PietRectangles where

-- TODO: Border widths in addition to fuzzes?

import Hylogen.WithHylide


black :: Vec4
black =
    vec4 (0.0, 0.0, 0.0, 1.0)

white :: Vec4
white =
    vec4 (1.0, 1.0, 1.0, 1.0)

transparent :: Vec4
transparent =
    vec4 (0.0, 0.0, 0.0, 0.0)

offwhite :: Vec4
offwhite =
    vec4 (249 / 256.0, 253 / 256.0, 245 / 256.0, 0.98)

red :: Vec4
red =
    vec4 (1.0, 0.0, 0.0, 1.0)

blue :: Vec4
blue =
    vec4 (0.0, 0.0, 1.0, 1.0)

green :: Vec4
green =
    vec4 (0.0, 1.0, 0.0, 1.0)

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

        pietRectangle fillColor position =
            rectangleSmooth black fillColor position borderWidth

        program =
            offwhite
                |> withLayer
                    (pietRectangle
                        (vec4 (1.0, 1.0, 0.0, 1.0))
                        (vec2 (0.665, 0.95))
                        0.33
                        0.25
                        )
                |> withLayer
                    (pietRectangle
                        red
                        (vec2 (0.1, 0.3))
                         0.37
                         0.2
                        )
                |> withLayer
                    (pietRectangle
                        transparent
                        (vec2 (0.2 - 2 * avgPixel, 0.1))
                         0.4
                         0.2
                         )
                |> withLayer
                    (pietRectangle
                        transparent
                        (vec2 (0.3, 0.65))
                        0.4
                        0.5
                        )
                |> withLayer
                    (pietRectangle
                        white
                        (vec2 (0.19, 0.65))
                        0.3
                        0.5
                        )
                |> withLayer
                    (pietRectangle
                        transparent
                        (vec2 (0.75, 0.3))
                        0.5
                        0.7
                        )
                |> withLayer
                    (pietRectangle
                        blue
                        (vec2 (0.449, 0.15))
                        0.101
                        0.1
                        )
                |> withLayer
                    (pietRectangle
                        white
                        (vec2 (0.875, 0.2))
                        0.30
                        0.60
                        )
                |> withLayer
                    (pietRectangle
                        black
                        (vec2 (0.665, 0.5))
                        0.33
                        0.15
                        )
    in
        toProgram program


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

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

opaque :: Vec3 -> Vec4
opaque c =
        vec4 (x_ c, y_ c, z_ c, 1.0)

smoothStep :: (Veccable v) => Vec v -> Vec v -> Vec v -> Vec v
smoothStep lo hi val =
        let
            clamped = clamp 0.0 1.0 ((val - lo) / (hi - lo))
        in
            clamped * clamped * ( 3 - 2 * clamped )


rectangleSmooth :: Vec4 -> Vec4 -> Vec2 -> Vec1 -> Vec1 -> Vec1 -> Vec4
rectangleSmooth color fillColor position fuzz width height =
    let
        finalColor =
            paintCorners
                $ paintLine
                    paintFill

        -- Translate Current Point to Center
        uv' =
            uv + (0.5 - position)


        -- Bottom Edge to Bottom Edge of Rectangle
        h' =
            (1 - height) / 2
        -- Left Edge to Left Edge of Rectangle
        w' =
            (1 - width) / 2


        isInside =
            (x_ uv' `gt` w')
                * (x_ uv' `lt` (1 - w'))
                * (y_ uv' `lt` (1 - h'))
                * (y_ uv' `gt` h')

        paintFill =
            sel isInside fillColor transparent


        -- Blend the color with an alpha
        blend alpha =
            vec4 (x_ color, y_ color, z_ color, w_ color * alpha)


        -- Perimeter Lines
        paintLine x =
            let
                alpha =
                    1 - (bottom * left * top * right)
                lineColor =
                    blend alpha
                amount' =
                    sel isInside
                        (withLayer lineColor fillColor) lineColor
            in
                sel (alpha `gt` 0.0) amount'
                    x

        bottom =
            limitX $ horizontalLine h'
        top =
            limitX $ horizontalLine (1 - h')
        left =
            limitY $ verticalLine w'
        right =
            limitY $ verticalLine (1 - w')

        horizontalLine =
            line y_

        verticalLine =
            line x_

        line selector val =
            1 - (smoothStep (val - fuzz) val (selector uv') -
                 smoothStep val (val + fuzz) (selector uv'))

        limitX =
            limitBy x_ w'

        limitY =
            limitBy y_ h'

        limitBy selector dimension val =
            sel (selector uv' `lt` dimension) 1.0
                $ sel (selector uv' `gt` (1 - dimension)) 1.0 val


        -- Corners
        paintCorners x =
            sel (isBottomCorner * isLeftCorner) paintBottomLeftCorner
                $ sel (isBottomCorner * isRightCorner) paintBottomRightCorner
                $ sel (isTopCorner * isRightCorner) paintTopRightCorner
                $ sel (isTopCorner * isLeftCorner) paintTopLeftCorner x

        isLeftCorner =
            (x_ uv' `lt` w')
                * (x_ uv' `gt` (w' - fuzz))
        isRightCorner =
            (x_ uv' `lt` (1 - w' + fuzz))
                * (x_ uv' `gt` (1 - w'))
        isTopCorner =
            (y_ uv' `lt` (1 - h' + fuzz))
                * (y_ uv' `gt` (1 - h'))
        isBottomCorner =
            (y_ uv' `lt` h')
                * (y_ uv' `gt` (h' - fuzz))

        paintBottomLeftCorner =
            paintCorner w' h'
        paintTopLeftCorner =
            paintCorner w' $ 1 - h'
        paintTopRightCorner =
            paintCorner (1 - w') $ 1 - h'
        paintBottomRightCorner =
            paintCorner (1 - w') h'

        paintCorner w h =
            let
                amount =
                    len (vec2 (w, h) - uv')
                        / fuzz
            in
                blend . clamp 0 1 $ 1 - amount
    in
        finalColor
