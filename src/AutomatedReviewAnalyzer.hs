module AutomatedReviewAnalyzer where

import Numeric.LinearAlgebra as LA
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import System.IO
import Data.Char (isAlpha, toLower)
import SentimentAnalysis

classify :: Matrix R -> Vector R -> R -> Vector R
classify x theta thata0 = cmap check (theta LA.<# LA.tr' x + theta0s)
    where
        theta0s = rows x LA.|> repeat 0
        check z = if z > 0 then 1.0 else -1.0 

accuracy :: Vector R -> Vector R -> R
accuracy pred target = sum xs / fromIntegral (length xs)
    where
        xs = zipWith check (toList pred) (toList target)
        check a b = if a == b then 1 else 0

classifierAccuracy :: (Matrix R -> Vector R -> Int -> [Int] -> (Vector R, R))
                            -> Matrix R -> Matrix R -> Vector R -> Vector R -> Int -> [Int] -> (R, R)
classifierAccuracy classifier x x' y y' t zs = 
    (accuracy (classify x theta theta0) y, accuracy (classify x' theta theta0) y')
    where
        (theta, theta0) = classifier x y t zs
 
classifierAccuracy' :: (Matrix R -> Vector R -> Int -> R -> [Int] -> (Vector R, R))
                            -> Matrix R -> Matrix R -> Vector R -> Vector R -> Int -> R -> [Int] -> (R, R)
classifierAccuracy' classifier x x' y y' t lambda zs = 
    (accuracy (classify x theta theta0) y, accuracy (classify x' theta theta0) y')
    where
        (theta, theta0) = classifier x y t lambda zs

separate :: String -> String
separate = concatMap separate'
    where 
        separate' c = if isAlpha c then [toLower c] else [' ', c, ' ']

addWord :: String -> M.Map String R -> M.Map String R
addWord w map = if M.member w map then map else M.insert w 1.0 map 

countWord :: String -> M.Map String R -> M.Map String R
countWord w = M.insertWith (+) w 1

extractFeatures :: [String] -> M.Map String R -> Matrix R
extractFeatures xs d = (length xss >< M.size d) ys
    where
        xss = (map words . map separate) xs
        ds = map (foldl (flip countWord) M.empty) xss
        ys = concatMap f $ zip ds (repeat d)
        f (d'', d') = map g $ map (flip M.lookup d'') $ M.keys d'
        g Nothing = 0
        g (Just a) = a

main :: IO ()
main = do

    h1 <- openFile "reviews_train.tsv" ReadMode
    hSetEncoding h1 latin1
    trainDta <- hGetContents h1
    h2 <- openFile "reviews_val.tsv" ReadMode
    hSetEncoding h2 latin1
    valDta <- hGetContents h2
--    testDta <- readFile "reviews_test.tsv"
    orderDta <- readFile "4000.txt"

    let t = 20
    let lambda = 0.005
    let (labels, text) = unzip $ map ((\ i -> ((read . head) i, i !! 4)) . splitOn "\t") (drop 1 $ lines trainDta)
    let bagOfWords =  foldr addWord M.empty $ (concatMap words . map separate) text
    let features = extractFeatures text bagOfWords
    let (labels', text') = unzip $ map ((\ i -> ((read . head) i, i !! 4)) . splitOn "\t") (drop 1 $ lines valDta)
    let features' = extractFeatures text' bagOfWords

    let indices = map read $ splitOn "," orderDta

    let (trainAccuracy, validationAccuracy) = classifierAccuracy perceptron features features' (vector labels) (vector labels') t indices
    let (trainAccuracy', validationAccuracy') = classifierAccuracy averagePerceptron features features' (vector labels) (vector labels') t indices
    let (trainAccuracy'', validationAccuracy'') = classifierAccuracy' pegasos features features' (vector labels) (vector labels') t lambda indices

    putStrLn "Classifier accuracy:"
--    print $ (takeColumns 10 . takeRows 4) features
    print $ "Training accuracy for perceptron: " ++ show trainAccuracy
    print $ "Validation accuracy for perceptron: " ++ show validationAccuracy
    print $ "Training accuracy for average perceptron: " ++ show trainAccuracy'
    print $ "Validation accuracy for average perceptron: " ++ show validationAccuracy'
    print $ "Training accuracy for pegasos: " ++ show trainAccuracy''
    print $ "Validation accuracy for pegasos: " ++ show validationAccuracy''

 
