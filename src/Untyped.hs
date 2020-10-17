module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe


import           Common

----------------------------------------------
-- Seccón 2
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion t = conversion' t []

conversion' :: LamTerm -> [String] -> Term
conversion' (LVar v) s = case elemIndex v s of Nothing -> Free (Global v)
                                               Just i -> Bound i
conversion' (App t1 t2) s = conversion' t1 s :@: conversion' t2 s
conversion' (Abs v t) s = Lam (conversion' t (v:s))

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp = undefined

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' _          _         = undefined


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined






