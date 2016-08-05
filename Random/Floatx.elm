module Random.Floatx exposing (..)

import Random exposing (Generator)

standardNormal : Generator Float
standardNormal = Random.map2 (\u theta -> sqrt (-2 * logBase e u) * cos theta)
  (Random.float 0 1) (Random.float 0 (pi+pi))
