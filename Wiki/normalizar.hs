import Data.Maybe
import Control.Monad

normalizar0 :: (Float, Float) -> (Float, Float)
normalizar0 (x, y) 
 | x /= 0 || y /= 0 = (x / n, y / n)
 | otherwise        = (1234, 5678)
 where n = sqrt (x * x + y * y)

normalizar1 :: (Float, Float) -> (Float, Float)
normalizar1 (x, y) 
 | x /= 0 || y /= 0 = (x / n, y / n)
 where n = sqrt (x * x + y * y)

normalizar2 :: (Float, Float) -> (Float, Float)
normalizar2 (x, y) 
 | x /= 0 || y /= 0 = (x / n, y / n)
 | otherwise        = undefined
 where n = sqrt (x * x + y * y)

-- #################################################################

normalizar3 :: (Float, Float) -> (Float, Float)
normalizar3 (x, y) 
 | x /= 0 || y /= 0 = (x / n, y / n)
 | otherwise = error "La función normalizar recibió un vector nulo!"
 where n = sqrt (x * x + y * y)

-- #################################################################

normalizar4 :: (Float, Float) -> Maybe (Float, Float)
normalizar4 (x, y)
  | x /= 0 || y /= 0 = Just (x / n, y / n)
  | otherwise        = Nothing
  where n = sqrt (x * x + y * y)


sumar :: (Float, Float) -> (Float, Float) -> (Float, Float)
sumar (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

ejemplo :: (Float, Float) -> (Float, Float) -> Maybe (Float, Float)
ejemplo v w = liftM2 sumar (normalizar4 v) (normalizar4 w)
