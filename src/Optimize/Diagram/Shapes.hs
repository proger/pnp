module Optimize.Diagram.Shapes where

import Diagrams.Prelude

cross, plus ::
  (V a ~ V2, Transformable a, TrailLike a, Semigroup a)
  => PrevDim (V a (N a)) -> a

cross x = (fromVertices [ x^&(-x) , ((-x)^&x) ]
          <> fromVertices [ x^&x , ((-x)^&(-x)) ])

plus x = cross x # rotate (45 @@ deg)
