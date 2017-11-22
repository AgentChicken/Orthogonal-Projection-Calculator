import Data.Vect
import Data.Matrix
import Data.Matrix.Numeric

infixl 8 <++>
(<++>) : Vect n Double -> Vect n Double -> Vect n Double
(<++>) v w = zipWith (+) v w

infixl 8 <~>
(<~>) : Vect n Double -> Vect n Double -> Vect n Double
(<~>) v w = zipWith (-) v w

infixl 8 <+++>
(<+++>) : Matrix n m Double -> Matrix n m Double -> Matrix n m Double
(<+++>) x y = zipWith (<++>) x y

orthogonalize : List (Vect m Double) -> List (Vect m Double)
orthogonalize xs = orthogonalize' xs []
  where
    projection : Vect n Double -> Vect n Double -> Vect n Double
    projection v u = ((u <:> v) / (u <:> u)) <# u
    orthogonalize' : List (Vect m Double) -> List (Vect m Double) -> List (Vect m Double)
    orthogonalize' [] acc = acc
    orthogonalize' (x::xs) [] = orthogonalize' xs [x]
    orthogonalize' (x::xs) acc = orthogonalize' xs (acc ++ [(x <~> (foldr1 (<++>) (map (projection x) acc)))])

projection_matrix : List (Vect m Double) -> Matrix m m Double
projection_matrix xs = foldr1 (<+++>) $ map (projection_matrix_onto_vect) $ orthogonalize xs
  where
    projection_matrix_onto_vect : Vect n Double -> Matrix n n Double
    projection_matrix_onto_vect v = (1 / (v <:> v)) <#> ((col v) <> (row v))
    
