module Optimize.Diagram where

import qualified Data.List as List
import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           System.Environment
import           System.Process

import Optimize.Diagram.Shapes

example :: [Point V2 Double] -> QDiagram B V2 Double Any
example pts
  = atPoints pts (repeat (circle 0.05 # fc green))

clusters :: [V.Vector (Point V2 Double)] -> [Point V2 Double] -> QDiagram B V2 Double Any
clusters cs centers = foldMap f $ zip (cycle cmap) (zip centers cs)
  where
    f :: (Colour Double, (Point V2 Double, V.Vector (Point V2 Double))) -> QDiagram B V2 Double Any
    f (color, (center, cluster))
      = atPoints (V.toList cluster)
        (repeat (circle 0.5 # fc color # lc color))
        <> moveTo center (cross 0.7 # fc color # lc color)

cmap :: [Colour Double]
cmap = [green, blue, red, orange, darkorchid, limegreen, lightskyblue, hotpink, lightcoral]

open files = spawnProcess "/usr/bin/open" ("-b":"com.google.Chrome":files)

plotClusters :: _ -> _ -> IO ()
plotClusters cs centers = do
  withArgs [ "-o"
           , "file.svg"
           , "-w"
           , "700"
           , "-h"
           , "600" ] (mainWith $ clusters cs centers)
  open ["file.svg"]
  return ()

somePoints = p2 <$> [(1,1), (0,3), (-2,1), (-1,-4), (2,0)]
