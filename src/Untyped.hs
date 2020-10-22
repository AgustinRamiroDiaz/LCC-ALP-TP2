module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe


import           Common

----------------------------------------------
-- Seccón 2
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

{-
Dado un término de lambda-cálculo (LamTerm) retorna su término equivalente
en la representación localmente sin nombres (Term)
-}

conversion :: LamTerm -> Term
conversion t = conversion' t []

{-
Función auxiliar que lleva una lista con las variables de ligadura
para poder reemplazar las variables ligadas por su índice de De Bruijn
-}

conversion' :: LamTerm -> [String] -> Term
conversion' (LVar v) s = case elemIndex v s of Nothing -> Free (Global v)
                                               Just i -> Bound i
conversion' (App t1 t2) s = conversion' t1 s :@: conversion' t2 s
conversion' (Abs v t) s = Lam (conversion' t (v:s))

-------------------------------
-- Sección 3
-------------------------------

{-
Dados dos valores ejecuta su aplicación
-}

vapp :: Value -> Value -> Value
vapp (VLam abs) v = abs v
vapp (VNeutral neutral) v = VNeutral (NApp neutral v)


{-
Devuelve el valor de evaluar el termino en el entorno
utilizando la estrategia de reducción normal
-}

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

{-
Función auxiliar que además lleva un entorno de variables locales
para la evaluación de variables localmente ligadas
-}

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free name) (nvs, _) = case find ((== name) . fst) nvs of Just (_, v) -> v
                                                                Nothing -> VNeutral (NFree name)
eval' (t1 :@: t2) e = vapp (eval' t1 e) (eval' t2 e)
eval' (Lam t) (nvs, lEnv) = VLam (\value -> eval' t (nvs, (value: lEnv)))

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

{-
Transforma un valor en término para poder mostrarlo
aplicando las funciones sobre variables frescas
-}

quote :: Value -> Term
quote = quote' 0

quote' :: Int -> Value -> Term
quote' i (VLam f) = Lam $ quote' (i + 1) (f (VNeutral (NFree (Quote i))))
quote' i (VNeutral (NFree name)) = case name of Global _ -> Free name
                                                Quote k -> Bound (i - k - 1)
quote' i (VNeutral (NApp neutral v)) = quote' i (VNeutral neutral) :@: quote' i v
