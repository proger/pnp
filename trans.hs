
import Data.SBV

production :: Goal
production = do
  x <- sInteger "X" -- Units of X produced
  y <- sInteger "Y" -- Units of X produced

  -- Amount of time on machine A and B
  let timeA = 50 * x + 24 * y
      timeB = 30 * x + 33 * y

  constrain $ timeA .<= 40 * 60
  constrain $ timeB .<= 35 * 60

  -- Amount of product we'll end up with
  let finalX = x + 30
      finalY = y + 90

  -- Make sure the demands are met:
  constrain $ finalX .>= 75
  constrain $ finalY .>= 95

  -- Policy: Maximize the final stock
  maximize "stock" $ (finalX - 75) + (finalY - 95)

main = optimize production
