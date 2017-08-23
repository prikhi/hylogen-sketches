{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
module Shapes where

import Hylogen.WithHylide

import Utils

data Shape
    = Rectangle
        { color :: Vec4
        , fillColor :: Vec4
        , position :: Vec2
        , fuzz :: Vec1
        , width :: Vec1
        , height :: Vec1
        }


draw :: Shape -> Vec4
-- Rectangles
draw Rectangle { color, fillColor, position, fuzz, width, height } =
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
