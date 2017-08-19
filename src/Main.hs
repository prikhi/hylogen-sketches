{-# LANGUAGE GADTs #-}
module Main where

import Hylogen.WithHylide

main :: IO ()
main = putStrLn "Run this like `stack exec hylide src/Main.hs`."


output :: Program
output = 
    toProgram playing

-- CH6 - HSB
hsb :: Vec4
hsb = 
    opaque . hsb2rgb $ vec3 (x_ uv, 1.0, y_ uv)

rgb2hsb :: Vec3 -> Vec3
rgb2hsb c =
    let
        k =
            vec4 (0.0, -1.0/3.0, 2.0/3.0, -1.0)
        p = 
            mix (step (b_ c) (g_ c)) 
                (vec4 (b_ c, g_ c, w_ k, z_ k))
                (vec4 (g_ c, b_ c, x_ k, y_ k))
        q = 
            mix (step (x_ p) (r_ c))
                (vec4 (x_ p, y_ p, w_ p, r_ c))
                (vec4 (r_ c, y_ p, z_ p, x_ p))
        d =
            x_ q - min_ (w_ q) (y_ q)
        e = 0.0000000001
    in 
        vec3 
            ( abs (z_ q + (w_ q - y_ q) / (6.0 * d + e))
            , d / (x_ q + e)
            , x_ q
            )

hsb2rgb :: Vec3 -> Vec3
hsb2rgb c =
    let
        rgb =
            clamp 0.0 1.0 $
                (abs ((xxx_ c * 6.0 + vec3 (0.0, 4.0, 2.0)) `mod_` 6.0) - 3.0) - 1.0
        rgb' =
            rgb * rgb * (3.0 - 2.0 * rgb)
    in
        zzz_ c * mix (y_ c) (copy 1.0) rgb'


opaque :: Vec3 -> Vec4
opaque c =
    vec4 (x_ c, y_ c, z_ c, 1.0)


step :: (Veccable v) => Vec v -> Vec v -> Vec v
step edge x =
    sel (x `lt` edge) (copy 0.0) (copy 1.0)


r_ :: Vec3 -> Vec1
r_ = x_

g_ :: Vec3 -> Vec1
g_ = y_

b_ :: Vec3 -> Vec1
b_ = z_



-- Messing Around

playing :: Vec4
playing = 
    let 
        result = calculateColor (x_ uv) $ y_ uv
    in
        vec4 (x_ result, y_ result, z_ result, 1)

calculateColor :: Vec1 -> Vec1 -> Vec3
calculateColor x _ =
    let
        (mouseX, mouseY) = (x_ mouse, y_ mouse)
    in
        gradientAndBack (log time) (vec3 (0.75, 0, mouseX)) (vec3 (mouseX, 0.75, mouseY)) x

gradientAndBack :: Vec1 -> Vec3 -> Vec3 -> Vec1 -> Vec3
gradientAndBack duration first second position =
    let
        shift = 
            (position + (time / duration)) `mod_` 2
    in
        sel (shift `lt` 1)
            (mix shift first second)
            (mix (shift - 1) second first)
