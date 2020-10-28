module NeuralNetwork where

import           Numeric.LinearAlgebra as LA
import           Text.Printf ( printf )
import           Numeric.Morpheus.Activation ( relu, reluGradient, sigmoid,
                    sigmoidGradient, tanh_, tanhGradient)

data Activation = Relu | Sigmoid | Tanh | Id

-- Neural network layer: weights, biases, and activation
data Layer a = Layer (Matrix a) (Matrix a) Activation

type NeuralNetwork a = [Layer a]

-- | Weight and bias gradients
data Gradients a = Gradients (Matrix a) (Matrix a)

getActivation :: Activation -> (Matrix Double -> Matrix Double)
getActivation Id = id
getActivation Sigmoid = sigmoid
getActivation Relu = relu
getActivation Tanh = tanh_

getActivation' :: Activation -> Matrix R -> Matrix R -> Matrix R
getActivation' Id = flip const
getActivation' Sigmoid = \x dY -> sigmoidGradient x * dY
getActivation' Relu = \x dY -> reluGradient x * dY
getActivation' Tanh = \x dY -> tanhGradient x * dY

bias' :: Matrix R -> Matrix R
bias' dY = cmap (/ m) r
  where
    r = matrix (cols dY) $ map sumElements (toColumns dY)
    m = fromIntegral $ rows dY

linearW' :: (Numeric t, Fractional t) => Matrix t -> Matrix t -> Matrix t
linearW' x dy = cmap (/ m) (tr' x LA.<> dy)
  where
    m = fromIntegral $ rows x

linearX' :: Numeric t => Matrix t -> Matrix t -> Matrix t
linearX' w dy = dy LA.<> tr' w

-- | Gradient descent optimization
optimize :: R -> Int -> [Layer R] -> (Matrix R, Matrix R) -> [Layer R]
optimize lr iterN net0 dataSet = last $ take iterN (iterate step net0)
  where
    step net = zipWith f net dW
        where
            (_, dW) = pass net dataSet

    f :: Layer R -> Gradients R -> Layer R
    f (Layer w b act) (Gradients dW dB) =
        Layer (w - lr `scale` dW) (b - lr `scale` dB) act

pass :: NeuralNetwork R -> (Matrix R, Matrix R) -> (Matrix R, [Gradients R])
pass net (x, tgt) = (pred, grads)
  where
    (_, pred, grads) = _pass x net

    _pass inp [] = (loss', pred, [])
      where
        pred = sigmoid inp
        loss' = pred - tgt

    _pass inp (Layer w b sact:layers) = (dX, pred, Gradients dW dB:t)
      where
        lin = (inp LA.<> w) + b
        y = getActivation sact lin

        (dZ, pred, t) = _pass y layers

        dY = getActivation' sact lin dZ
        dW = linearW' inp dY
        dB = bias' dY
        dX = linearX' w dY

data AdamParameters = AdamParameters { _beta1 :: R, _beta2 :: R, _epsilon :: R, _lr :: R }

adamParams :: AdamParameters
adamParams = AdamParameters { _beta1 = 0.9
                            , _beta2 = 0.999
                            , _epsilon = 1e-8
                            , _lr = 0.001
                            }

-- | Adam optimization
optimizeAdam :: AdamParameters -> Int -> [Layer R] -> (Matrix R, Matrix R) -> [Layer R]
optimizeAdam p iterN w0 dataSet = w
  where
    s0 = map zf w0
    v0 = map zf w0
    zf (Layer a b _) = (zerosLike a, zerosLike b)
    zerosLike m = matrix c (replicate (r*c) 0.0)
      where
        r = rows m
        c = cols m
    (w, _, _) = adam p iterN (w0, s0, v0) dataSet
    
adam :: AdamParameters -> Int -> ([Layer R], [(Matrix R, Matrix R)], [(Matrix R, Matrix R)]) -> (Matrix R, Matrix R) -> ([Layer R], [(Matrix R, Matrix R)], [(Matrix R, Matrix R)])
adam p@AdamParameters { _lr = lr, _beta1 = beta1, _beta2 = beta2, _epsilon = epsilon } iterN (w0, s0, v0) dataSet = last $ take iterN (iterate step (w0, s0, v0))
      where
        step (w, s, v) = (wN, sN, vN)
          where
            (_, dW) = pass w dataSet

            sN = zipWith f2 s dW
            vN = zipWith f3 v dW
            wN = zipWith3 f w vN sN

        f (Layer w_ b_ sf) (vW, vB) (sW, sB) =
           Layer (w_ - lr `scale` vW / (sqrt sW `addC` epsilon))
                 (b_ - lr `scale` vB / (sqrt sB `addC` epsilon))
                 sf

        addC m c = cmap (+ c) m

        f2 (sW, sB) (Gradients dW dB) =
          ( beta2 `scale` sW + (1 - beta2) `scale` (dW^2)
          , beta2 `scale` sB + (1 - beta2) `scale` (dB^2))

        f3 (vW, vB) (Gradients dW dB) =
          ( beta1 `scale` vW + (1 - beta1) `scale` dW
          , beta1 `scale` vB + (1 - beta1) `scale` dB)

genWeights :: (Int, Int) -> IO (Matrix Double, Matrix Double)
genWeights (nin, nout) = do
  w <- _genWeights (nin, nout)
  b <- _genWeights (1, nout)
  return (w, b)
    where
      _genWeights (nin, nout) = do
          let k = sqrt (1.0 / fromIntegral nin)
          w <- randn nin nout
          return (k `scale` w)

makeCircles :: Int -> R -> R -> IO (Matrix R, Matrix R)
makeCircles m factor noise = do
  let rand' n = scale (2 * pi) <$> rand n 1
      m1 = m `div` 2
      m2 = m - (m `div` 2)

  r1 <- rand' m1
  r2 <- rand' m2
  ns <- scale noise <$> randn m 2

  let outerX = cos r1
      outerY = sin r1
      innerX = scale factor $ cos r2
      innerY = scale factor $ sin r2
      -- Merge them all
      x = fromBlocks [[outerX, outerY], [innerX, innerY]]

      -- Labels
      y1 = m1 >< 1 $ repeat 0
      y2 = m2 >< 1 $ repeat 1
      y = y1 === y2

  return (x + ns, y)

makeSpirals :: Int -> R -> IO (Matrix R, Matrix R)
makeSpirals m noise = do
  r0 <- scale (780 * 2 * pi / 360) . sqrt <$> rand m 1
  d1x0 <- scale noise <$> rand m 1
  d1y0 <- scale noise <$> rand m 1

  let d1x = d1x0 - cos r0 * r0
  let d1y = d1y0 + sin r0 * r0

  let x = fromBlocks [[d1x, d1y], [- d1x, - d1y]] / 10.0
  let y1 = m >< 1 $ repeat 0
  let y2 = m >< 1 $ repeat 1
  let y = y1 === y2
  return (x, y)

accuracy :: [Layer R] -> (Matrix R, Matrix R) -> R
accuracy net (dta, tgt) = 100 * (1 - e / m)
  where
    pred = net `inferBinary` dta
    e = sumElements $ abs (tgt - pred)
    m = fromIntegral $ rows tgt

inferBinary :: NeuralNetwork R -> Matrix R -> Matrix R
inferBinary net dta =
  let pred = forward net dta
  in cmap (\a -> if a < 0.5 then 0 else 1) pred

forward :: NeuralNetwork R -> Matrix R -> Matrix R
forward net dta = fst $ pass net (dta, undefined)

genNetwork :: [Int] -> [Activation] -> IO [Layer R]
genNetwork nodes activations = do
    weights <- mapM genWeights nodes'
    return (zipWith (\(w, b) a -> Layer w b a) weights activations)
  where
    nodes' = zip nodes (tail nodes)

experiment1 :: IO ()
experiment1 = do
  trainSet <- makeCircles 200 0.6 0.1
  testSet <- makeCircles 100 0.6 0.1

  let (dta, tgt) = trainSet

  (w1_rand, b1_rand) <- genWeights (2, 128)
  (w2_rand, b2_rand) <- genWeights (128, 1)

  let net = [ Layer w1_rand b1_rand Relu
            , Layer w2_rand b2_rand Id ]

  -- -- Alternatively:
  -- net0 <- genNetwork [2, 128, 1] [Relu, Id]

  let epochs = 1000
      lr = 0.001  -- Learning rate
      net' = optimize lr epochs net trainSet
      netA = optimizeAdam adamParams epochs net trainSet

  putStrLn $ printf "Circles problem, 1 hidden layer of 128 neurons, %d epochs" epochs
  putStrLn "---"

  putStrLn $ printf "Training accuracy (gradient descent) %.1f" (net' `accuracy` trainSet)
  putStrLn $ printf "Validation accuracy (gradient descent) %.1f\n" (net' `accuracy` testSet)

  putStrLn $ printf "Training accuracy (Adam) %.1f" (netA `accuracy` trainSet)
  putStrLn $ printf "Validation accuracy (Adam) %.1f\n" (netA `accuracy` testSet)

  putStrLn ""

experiment2 :: IO ()
experiment2 = do
  trainSet <- makeSpirals 200 0.5
  testSet <- makeSpirals 100 0.5

  let (dta, tgt) = trainSet
      epochs = 700

  putStrLn $ printf "Spirals problem, Adam, %d epochs" epochs
  putStrLn "---"
  putStrLn "1 hidden layer, 128 neurons (513 parameters)"
  net0 <- genNetwork [2, 128, 1] [Relu, Id]
  let net0' = optimizeAdam adamParams epochs net0 trainSet

  putStrLn $ printf "Training accuracy %.1f" (net0' `accuracy` trainSet)
  putStrLn $ printf "Validation accuracy %.1f\n" (net0' `accuracy` testSet)

  putStrLn "1 hidden layer, 512 neurons (2049 parameters)"
  net1 <- genNetwork [2, 512, 1] [Relu, Id]
  let net1' = optimizeAdam adamParams epochs net1 trainSet

  putStrLn $ printf "Training accuracy %.1f" (net1' `accuracy` trainSet)
  putStrLn $ printf "Validation accuracy %.1f\n" (net1' `accuracy` testSet)

  putStrLn "3 hidden layers, 40, 25, and 10 neurons (1416 parameters)"
  net2 <- genNetwork [2, 40, 25, 10, 1] [Relu, Relu, Relu, Id]
  let net2' = optimizeAdam adamParams epochs net2 trainSet

  putStrLn $ printf "Training accuracy %.1f" (net2' `accuracy` trainSet)
  putStrLn $ printf "Validation accuracy %.1f\n" (net2' `accuracy` testSet)

main :: IO ()
main = experiment1 >> experiment2


