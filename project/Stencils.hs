{-# LANGUAGE QuasiQuotes #-}

module Stencils
(
laplaceStencil,
sobelXStencil,
sobelYStencil,
highpassStencil2,
highPassStencil,
gaussStencil,
isBoundary,
applyStencil
) where 



import Data.Array.Repa (Array,delay, DIM1, DIM2,DIM3, U, D, Z (..), (:.)(..), (!))
import Data.Array.Repa.Stencil.Dim2 
import qualified Data.Array.Repa.Stencil as A
import Language.Haskell.TH.Quote (QuasiQuoter)

type Channel = Array D DIM2 Float

laplaceStencil:: A.Stencil DIM2 Float
laplaceStencil = [stencil2| -1 -1 -1 -1 -1
							-1 -1 -1 -1 -1 
							-1 -1 24 -1 -1
							-1 -1 -1 -1 -1
							-1 -1 -1 -1 -1|]

sobelXStencil :: A.Stencil DIM2 Float
sobelXStencil = [stencil2| -1 0 1
						  -2 0 2
						  -1 0 1|]

sobelYStencil :: A.Stencil DIM2 Float
sobelYStencil = [stencil2| -1 -2 -1
							0  0  0
							1  2  1 |]


highPassStencil :: A.Stencil DIM2 Float 
highPassStencil = [stencil2| -11 -11 -11
						  -11 -11 -11
						  -11 -11 -11 |]
highpassStencil2:: A.Stencil DIM2 Float
highpassStencil2 = [stencil2| 0 -25 0
							 -25 200 -25
							  0 -25 0 |]


gaussStencil :: A.Stencil DIM2 Float 
gaussStencil = [stencil2| 2 4 5 4 2
						  4 9 12 9 4
						  5 12 15 12 5
						  4 9 12 9 4
						  2 4 5 4 2 |]

applyStencil :: A.Stencil DIM2 Float -> Channel -> Channel
applyStencil stencil arr = delay $ mapStencil2 (A.BoundConst 0) stencil arr

isBoundary:: Int -> Int -> DIM3 -> Bool
isBoundary length breadth (Z:.a:.b:.c) = (a>= length-1 || a==0) || (b>=breadth-1 || b==0)


