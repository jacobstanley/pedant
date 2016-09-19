{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}

--
-- Some comments
--

data     Foo     a b =      Bar !a   |     Baz [
  b]

data        Booly  =
                            forall a.
                    Yay   
                   
                   
                   !Int
         |
         
         Nah      
         
         
         Bool 

         !(m ())
         
         
    deriving (Eq, Ord, Show)

foo :: Int -> Foo
foo = bar
