module Party where

import Employee
import Data.Monoid

glCons :: Employee -> GuestList -> GuestList
glCons e (GL gl fun)  = GL (e:gl) ((empFun e) + fun)

instance Monoid GuestList where
    mempty = GL [] 0
    (GL e1 f1) `mappend` (GL e2 f2) = GL (e1++e2) (f1+f2) 

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
        | f1 > f2   = gl1
        | otherwise = gl2
