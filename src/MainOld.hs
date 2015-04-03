{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Control.Monad
import           Data.Ratio

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import           NumericPrelude


main :: IO ()
main = startGUI defaultConfig setup

canvasSide = 416
center = (fromIntegral canvasSide / 2, fromIntegral canvasSide / 2)
radius = 100

radian angle = angle * pi / 180

{-
data PState = PState { time   :: Double
                     , posx   :: Double
                     , posx'  :: Double
                     , theta  :: Double
                     , theta' :: Double
                     , length :: Double
                     , mass1  :: Double
                     , mass2  :: Double
                     }
            deriving (Show, Read)

deltaT :: Double
deltaT = 0.1

grav :: Double
grav = 100 -- 9.81

accel :: Double -> Double
accel th = grav * sin (radian th)
-}

data SState = SState { sstateT      :: Double
                     , sstateX      :: Double
                     , sstateTheta  :: Double
                     , sstateX'     :: Double
                     , sstateTheta' :: Double
                     , sstateForce  :: Double
                     } deriving (Eq, Show, Read)

data SConfig = SConfig { step    :: Double
                       , xF      :: SConfig -> SState -> Double
                       , thetaF  :: SConfig -> SState -> Double
                       , xF'     :: SConfig -> SState -> Double
                       , thetaF' :: SConfig -> SState -> Double
                       , m1      :: Double
                       , m2      :: Double
                       , r       :: Double
                       , g       :: Double
                       , iState  :: SState
                       }

initialCond :: SState
initialCond = SState 0 0 startAngle 0 0 0
  where
    startAngle = 45

setForce :: Double -> SState -> SState
setForce f' i = i { sstateForce = f' }

sim :: SConfig
sim = SConfig { step    = 0.1
              , xF      = const sstateX'
              , thetaF  = const sstateTheta'
              , xF'     = vfunc
              , thetaF' = wfunc
              , m1      = 1
              , m2      = 1
              , r       = 10
              , g       = 9.81
              , iState  = initialCond
              }
  where
    vfunc (SConfig { m1 = m, step = h }) (SState _ _ _ v _ f)
      = f + h*(f / m)
    wfunc (SConfig { r = rad, g = grav, m1 = m }) (SState _ _ theta v w f)
      = sqrt ((grav - ((f * (cos theta - 1)) / (m * sin theta))) / rad)

update :: SConfig -> SState -> SState
update cfg state = SState t_ x_ theta_ v_ w_ f
    where
    h = step cfg
    (SState t x theta v w f) = state
    (SConfig _ f_x f_theta f_v f_w _ _ _ _ _) = cfg
    fl = map ($ cfg) [f_x, f_theta, f_v, f_w]
    h_ = h/2
    h__ = h/6
    helper' k [a, b, c, d] = SState (t + k) (x + a) (theta + b) (v + c) (w + d) f
    helper k i = helper' k (map (/2) i)
    s1 = helper 0  [0, 0, 0, 0]
    s2 = helper h_ [k1a, k1b, k1c, k1d]
    s3 = helper h_ [k2a, k2b, k2c, k2d]
    s4 = helper h  [k3a, k3b, k3c, k3d]
    [k1a, k1b, k1c, k1d] = map ((*h) . ($ s1)) fl
    [k2a, k2b, k2c, k2d] = map ((*h) . ($ s2)) fl
    [k3a, k3b, k3c, k3d] = map ((*(2*h)) . ($ s3)) fl
    [k4a, k4b, k4c, k4d] = map ($ s4)          fl
    x_     = x + (k1a + 2 * k2a + 2 * k3a + k4a) * h__
    theta_ = theta + (k1b + 2 * k2b + 2 * k3b + k4b) * h__
    v_     = v + (k1c + 2 * k2c + 2 * k3c + k4c) * h__
    w_     = w + (k1d + 2 * k2d + 2 * k3d + k4d) * h__
    t_     = t + h

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

renderPend
  canvas
  (SConfig { m1 = m, m2 = m', r = l })
  (SState _ x th _ _ _)
   = drawPend canvas x l th m m'

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

