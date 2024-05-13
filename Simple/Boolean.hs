module Boolean(Boolean(..)) where

import Prelude hiding ((&&), (||), not)

class (Eq b) => Boolean b where
    false :: b 
    true  :: b

    (&&) :: b -> b -> b
    a && b = if (a,b) == (true,true)
             then true
             else false

    (||) :: b -> b -> b
    a || b = if (a,b) == (false,false)
             then false
             else true

    not   :: b -> b
    not a = if a == true 
            then false
            else true  

    xor   :: b -> b -> b
    xor a b = if a == b
              then false
              else true 

    nand  :: b -> b -> b
    nand a b = not $ a && b

    nor :: b -> b -> b
    nor a b = not $ a || b

    xnor :: b -> b -> b
    xnor a b = not $ xor a b

    

instance Boolean Bool where
    false = False
    true = True