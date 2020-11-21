module NN where

import Numeric.LinearAlgebra as LA

rectifiedLinearUnit :: R -> R
rectifiedLinearUnit = max 0

rectifiedLinearUnit' :: R -> R
rectifiedLinearUnit' x = if rectifiedLinearUnit x == 0 then 0 else 1

trainNeuralNetwork :: [((R, R), R)] -> Matrix R -> Matrix R -> Matrix R -> R -> Int -> (Matrix R, Matrix R, Matrix R)
trainNeuralNetwork xys wT b vT learningRate epochs = iterate (step xys learningRate) (wT, b, vT) !! epochs
    where
        step xys learningRate (wT, b, vT) = train xys wT b vT learningRate 

train :: [((R, R), R)] -> Matrix R -> Matrix R -> Matrix R -> Double -> (Matrix R, Matrix R, Matrix R)
train xys wT b vT learningRate = train' xys wT b vT learningRate
    where
        train' [] wTs b vTs _ = (wTs, b, vTs)
        train' (((x1,x2), y):xys') wTs b vTs learningRate = 
            let (wTs', b', vTs') = trainPoint wTs b vTs learningRate x1 x2 y
            in train' xys' wTs' b' vTs' learningRate

testNeuralNetwork :: [(R, R)] -> Matrix R -> Matrix R -> Matrix R -> R -> [R]
testNeuralNetwork xs wT b vT learningRate = map (calc wT b vT learningRate) xs
    where
        calc wT b vT learningRate (x1, x2) = predict wT b vT learningRate x1 x2

trainPoint :: Matrix R -> Matrix R -> Matrix R -> R -> R -> R -> R -> (Matrix R, Matrix R, Matrix R)
trainPoint wT b vT learningRate x1 x2 y = (wT - scalar learningRate * dw, b - scalar learningRate * db, vT - scalar learningRate * dv)
    where
        x = (2 >< 1) [x1, x2]
        
        z = wT LA.<> x + b
        a =  cmap rectifiedLinearUnit z

        u =  vT LA.<> a

        du = u - scalar y
        dv = du LA.<> tr a
        da = tr vT LA.<> du
        dz = da * cmap rectifiedLinearUnit' z
        
        db = dz
        dw = dz LA.<> tr x

predict :: Matrix R -> Matrix R -> Matrix R -> R -> R -> R -> R
predict wT b vT learningRate x1 x2  = u `atIndex` (0,0)
    where
        z = wT LA.<> (2 >< 1) [x1, x2] + b
        a = cmap rectifiedLinearUnit z
        u = vT LA.<> a
