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
vapp (VLam abs) v = abs v
vapp (VNeutral neutral) v = VNeutral (NApp neutral v)
-- vapp (VNeutral neutral@(NFree _)) v = VNeutral (NApp neutral v)
-- vapp (VNeutral (NApp neutral v)) v' = VNeutral (NApp neutral (vapp v v'))
-- ((neutral v) v')               ->      (neutral (v v'))

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free name) (nvs, _) = case find ((== name) . fst) nvs of Just (_, v) -> v
                                                                Nothing -> VNeutral (NFree name)
eval' (t1 :@: t2) e = vapp (eval' t1 e) (eval' t2 e)
eval' (Lam t) (nvs, lEnv) = VLam (\value -> eval' t (nvs, (value: lEnv)))

-- -- (\x . (y 2 \x3) y z) (y 2 3) --> y

-- eval' _          _         = undefined

-- eval' (Lam (Bound 0) :@: Free (Global "y")) ([], []) == Free (Global "y")
    -- eval' (Lam (Bound 0)) ([], []) == Lam (\x -> x)
    -- eval' (Free (Global "y")) ([], []) == VNeutral (NFree (Global "y"))
    -- vapp (Lam (\x -> x))


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = quote' 0

quote' :: Int -> Value -> Term
quote' i (VLam f) = case f (VNeutral (NFree (Quote i))) of VNeutral (NFree (Quote k)) -> Bound (i - k - 1)
                                                           v -> quote' (i+1) v
quote' _ (VNeutral (NFree name)) = Free name
quote' i (VNeutral (NApp neutral v)) = quote' i (VNeutral neutral) :@: quote' i v
