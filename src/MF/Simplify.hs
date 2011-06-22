module MF.Simplify where
    
import MF.F
-- Implement Simplify for a language if you would like to use one of the analysis in this package.
    

class Simplify a b where
    simplify :: a -> b