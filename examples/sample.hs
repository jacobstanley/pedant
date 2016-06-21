{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}

--
-- Some comments
--

data     Foo      =      Bar    |     Baz

data        Booly  =
                    Yay   
                   
                   
                   !Int
         |
         
         Nah      
         
         
         Bool 

         !(m ())
         
         
         deriving (Eq, Ord, Show)

foo :: Int -> Foo
foo = bar
