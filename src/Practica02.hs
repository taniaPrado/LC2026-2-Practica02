module Practica02 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

--EJERCICIOS

--Ejercicio 1
variables :: Prop -> [String]
variables prop = varsAux prop []
  where
    varsAux :: Prop -> [String] -> [String]
    varsAux (Var x) collector
        | include x collector = collector
        | otherwise = collector ++ [x]
    varsAux (Cons _) collector = collector
    varsAux (Not f) collector  = varsAux f collector
    varsAux (And f g) collector = varsAux g (varsAux f collector )
    varsAux (Or  f g) collector = varsAux g (varsAux f collector)
    varsAux (Impl f g) collector = varsAux g (varsAux f collector)
    varsAux (Syss f g) collector = varsAux g (varsAux f collector)

--Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons a) _ = a
interpretacion (Var x) list = include x list
interpretacion (Not prop) list = not (interpretacion prop list)
interpretacion (And x y) list = (interpretacion y list) && (interpretacion x list)
interpretacion (Or x y) list = (interpretacion x list) || (interpretacion y list) 
interpretacion (Impl x y) list = (interpretacion (Not x) list) || (interpretacion y list)
interpretacion (Syss x y) list = (interpretacion x list) == (interpretacion y list)

--Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles prop = conjuntoPotencia (variables prop)

--Ejercicio 4
modelos :: Prop -> [Estado]
modelos prop = modelosRecur prop (estadosPosibles prop) []
    where
        modelosRecur :: Prop -> [Estado] -> [Estado] -> [Estado]
        modelosRecur _ [] collector = collector
        modelosRecur prop (x:xs) collector 
            | interpretacion prop x = modelosRecur prop xs (collector ++ [x])
            | otherwise             = modelosRecur prop xs collector


--Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes (Cons True) (Cons True) = True
sonEquivalentes (Cons True) q = sonEquivalentesRecu (Cons True) q (estadosPosibles q)
sonEquivalentes p (Cons True) = sonEquivalentesRecu p (Cons True) (estadosPosibles p)
sonEquivalentes p q = (sonEquivalentesRecu p q (estadosPosibles p)) && (sonEquivalentesRecu p q (estadosPosibles q))

sonEquivalentesRecu :: Prop -> Prop -> [Estado] -> Bool
sonEquivalentesRecu _ _ [] = True
sonEquivalentesRecu p q (x:xs) 
    | interpretacion (Syss p q) x = sonEquivalentesRecu p q xs
    | otherwise                   = False

--Ejercicio 6 
tautologia :: Prop -> Bool
tautologia p = sonEquivalentes p (Cons True)

--Ejercicio 7
contradiccion :: Prop -> Bool
contradiccion = undefine

--Ejercicio 8
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica props prop = verifica prop (modelos (conjunciones props)) 
    where
        verifica :: Prop -> [Estado] -> Bool
        verifica prop [] = True
        verifica prop (x:xs)
            | interpretacion prop x = verifica prop xs
            | otherwise             = False

--Funion que hace la conjuncion de una lista de proposiciones
conjunciones :: [Prop] -> Prop
conjunciones [] = Cons True
conjunciones (x:[]) = x
conjunciones (x:xs) = And (x) (conjunciones xs)


--Funcion auxiliar
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs

include :: (Eq a) => a -> [a] -> Bool
include _ [] = False
include elem (head : sublist)
    | elem == head = True
    | otherwise    = include elem sublist
