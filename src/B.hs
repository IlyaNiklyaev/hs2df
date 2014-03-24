module B where

data TestData = First Int | Second Bool

gen :: Int -> TestData
gen 0 = Second False
gen 1 = Second True
gen n = First n

main :: Int -> Bool
main x = case gen x of
        First _ -> True
        Second _ -> False

--cb :: Float
--cb = 0.0083
--c :: Float
--c = 0.0083
--mux :: Float
--mux = 0.1
--muy :: Float
--muy = 0.1
--eta :: Float
--eta = 86.8
--n :: Float
--n = 86.8
--ul :: Float
--ul = 0.1
--m :: Float
--m = 1
--j :: Float
--j = 0.006
--g :: Float
--g = 9.8
--ur :: Float
--ur = 100
--r :: Float
--r = 0.03
--l :: Float
--l = 0.245
--d :: Float
--d = 0.1
--cv :: Float
--cv = 0.1
--cw :: Float
--cw = 0.1
--vc :: Float
--vc = 0.01
--wc :: Float
--wc = 0.01
--
--sgn :: Float -> Float
--sgn = signum
--
--ftr :: Float -> Float
--ftr v = - sgn (v) * mux * m * g
--
--mtr :: Float -> Float
--mtr w = - sgn (w) * muy * m * g * l / 4.0
--
--ft :: Float -> Float -> Float
--ft i1 i2 = c * n  / r * (i1 + i2)
--
--mt :: Float -> Float -> Float
--mt i1 i2 = c * n * d / r * (i2 - i1)
--
--v' :: Float -> Float -> Float -> Float -> Float -> Float
--v' t v w i1 i2 = if abs (ft i1 i2) < abs (ftr v) && sgn (ft i1 i2) == sgn (ftr v) && abs (v) < 0.001 then -cv/m else (ft i1 i2 + ftr v)/m
--
--w' :: Float -> Float -> Float -> Float -> Float -> Float
--w' t v w i1 i2 = if abs (mt i1 i2) < abs (mtr w) && sgn (mt i1 i2) == sgn (mtr w) && abs (w) < 0.001 then -cw/j else (mt i1 i2 + mtr w)/j
--
--i1' :: Float -> Float -> Float -> Float -> Float -> Float -> Float
--i1' u1 t v w i1 i2 = (-ur * i1 - cb * n / r * (v - d * w) + u1) / ul
--
--i2' :: Float -> Float -> Float -> Float -> Float -> Float -> Float
--i2' u2 t v w i1 i2 = (-ur * i2 - cb * n / r * (v + d * w) + u2) / ul
--
--rk4adopt4 ::
--        Float ->
--        (Float -> Float -> Float -> Float -> Float -> Float) ->
--        (Float -> Float -> Float -> Float -> Float -> Float) ->
--        (Float -> Float -> Float -> Float -> Float -> Float) ->
--        (Float -> Float -> Float -> Float -> Float -> Float) ->
--        (Float, Float, Float, Float, Float) ->
--        (Float, Float, Float, Float, Float)
--rk4adopt4 h f1 f2 f3 f4 (t, x1, x2, x3, x4) =
--        (t + h,
--        x1 + (1/6) * (k1 + 2*k2 + 2*k3 + k4),
--        x2 + (1/6) * (l1 + 2*l2 + 2*l3 + l4),
--        x3 + (1/6) * (m1 + 2*m2 + 2*m3 + m4),
--        x4 + (1/6) * (n1 + 2*n2 + 2*n3 + n4))
--        where
--                k1 = h * f1 t x1 x2 x3 x4
--                l1 = h * f2 t x1 x2 x3 x4
--                m1 = h * f3 t x1 x2 x3 x4
--                n1 = h * f4 t x1 x2 x3 x4
--                
--                k2 = h * f1 (t + h/2) (x1 + k1/2) (x2 + l1/2) (x3 + m1/2) (x4 + n1/2)
--                l2 = h * f2 (t + h/2) (x1 + k1/2) (x2 + l1/2) (x3 + m1/2) (x4 + n1/2)
--                m2 = h * f3 (t + h/2) (x1 + k1/2) (x2 + l1/2) (x3 + m1/2) (x4 + n1/2)
--                n2 = h * f4 (t + h/2) (x1 + k1/2) (x2 + l1/2) (x3 + m1/2) (x4 + n1/2)
--                
--                k3 = h * f1 (t + h/2) (x1 + k2/2) (x2 + l2/2) (x3 + m2/2) (x4 + n2/2)
--                l3 = h * f2 (t + h/2) (x1 + k2/2) (x2 + l2/2) (x3 + m2/2) (x4 + n2/2)
--                m3 = h * f3 (t + h/2) (x1 + k2/2) (x2 + l2/2) (x3 + m2/2) (x4 + n2/2)
--                n3 = h * f4 (t + h/2) (x1 + k2/2) (x2 + l2/2) (x3 + m2/2) (x4 + n2/2)
--                
--                k4 = h * f1 (t + h) (x1 + k3) (x2 + l3) (x3 + m3) (x4 + n3)
--                l4 = h * f2 (t + h) (x1 + k3) (x2 + l3) (x3 + m3) (x4 + n3)
--                m4 = h * f3 (t + h) (x1 + k3) (x2 + l3) (x3 + m3) (x4 + n3)
--                n4 = h * f4 (t + h) (x1 + k3) (x2 + l3) (x3 + m3) (x4 + n3)