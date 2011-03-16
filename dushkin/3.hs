module D3 where

--3.1
data Complex = Complex
    {
      realPart :: Float,
      imagPart :: Float
    } deriving Show

instance Eq Complex where
    Complex r1 i1 == Complex r2 i2 = (r1==r2) && (i1==i2)

instance Num Complex where
    (Complex r1 i1) + (Complex r2 i2) = Complex (r1+r2) (i1+i2)
    (Complex r1 i1) - (Complex r2 i2) = Complex (r1-r2) (i1-i2)
    (Complex r1 i1) * (Complex r2 i2) = Complex (r1*r2-i1*i2) (r1*i2+r2*i1)
    negate (Complex r i)              = Complex (negate r) (negate 

                                                            
--3.3
class Logic a where
(&&) :: a->a->a
(||) :: a->a->a
neg :: a->a     

    
                                   

