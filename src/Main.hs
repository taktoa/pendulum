{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Control.Monad
import           Data.Ratio

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import           Data.Foldable
import           Data.Monoid                 (mempty)

import           Linear.Matrix
import           Numeric.AD

canvasSide :: Int
canvasSide = 416

center :: UI.Point
center = (fromIntegral canvasSide / 2, fromIntegral canvasSide / 2)

radian :: Floating a => a -> a
radian angle = angle * pi / 180

{-
instance Num a => Num (b -> a) where
  f + g         = \x -> f x + g x
  f * g         = \x -> f x * g x
  abs f         = abs . f
  signum f      = signum . f
  fromInteger n = const (fromInteger n)
  negate f      = negate . f

instance (Num a, Num b) => Num (a, b) where
  (a, b) + (c, d) = (a + c, b + d)
  (a, b) - (c, d) = (a - c, b - d)
  (a, b) * (c, d) = (a * c, b * d)
  abs (a, b)      = (abs a, abs b)
  signum (a, b)   = (signum a, signum b)
  fromInteger n   = (fromInteger n, fromInteger n)
-}
-- u[i + 1] = u[i] + u'[i]*dt
-- u'[i + 1] = u'[i] + (laplace u)*dt

--laplace :: Num a => SState v a -> (a, a) -> [a]
f <.> g = \x -> f <$> g x

laplace = trace . hessian

data SConfig v a = SConfig { sos    :: v a -> a
                           , length :: Integer
                           }

data SState v a = SState { height  :: v a -> a
                         , height' :: a -> a -> a
                         , time    :: Integer
                         }

main = putStrLn "hi"


--main :: IO ()
--main = startGUI defaultConfig setup

{-
initialCond :: SState

sim :: SConfig

update :: SConfig -> SState -> SState

drawPend :: SState -> UI ()
drawPend canvas px l th m m' = do
  canvas # UI.beginPath
  canvas # UI.moveTo pos1
  canvas # UI.lineTo pos2
  canvas # UI.closePath
  canvas # UI.stroke

  canvas # UI.beginPath
  canvas # UI.arc pos1 (scale m) 0 (2 * pi)
  canvas # UI.closePath
  canvas # UI.fill

  canvas # UI.beginPath
  canvas # UI.arc pos2 (scale m') 0 (2 * pi)
  canvas # UI.closePath
  canvas # UI.fill

  where
    pos1 = center `add` (scale px, 0)
    pos2 = center `add` (scale px, 0) `add` pol (scale l) (90 - th)
    pol d t = let r = radian t in d `times` (cos r, sin r)
    add (a, b) (x, y) = (a + x, b + y)
    times s (x, y) = (s * x, s * y)
    scale x = 10 * x

setup :: Window -> UI ()
setup window = do
    return window # set title "Pie Chart"

    canvas <- UI.canvas
        # set UI.height canvasSide
        # set UI.width  canvasSide
        # set style [("border", "solid black 1px"), ("background", "lightgrey")]
        # set UI.lineWidth 2
        # set UI.strokeStyle "white"

    pieState <- UI.textarea
        # set UI.type_ "text"
        # set UI.rows "10"
        # set UI.value (show initialCond)
    pieTimer <- UI.timer
        # set UI.interval 100

    let getState = do
          p <- get UI.value pieState
          return (read p :: SState)

    let setState p = return pieState # set UI.value (show p)

    let modState f = getState >>= setState . f

    getBody window #+
      [ column [element canvas, element pieState]
      ]

    UI.start pieTimer

    on UI.tick pieTimer $ const $ do
      ps <- getState
      setState (update' ps)
      UI.clearCanvas canvas
      renderPend canvas sim ps

    on UI.mousemove canvas $ \(x, _) -> modState $ setForce (fromIntegral x - fst center)

    on UI.keyup canvas $ const $ modState (setForce 0)

  where
    update' = update sim

-}
