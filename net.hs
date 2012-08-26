module Main where
import Random
import Data.Traversable hiding (forM)
import Control.Monad

import Debug.Trace

import Genet

type    Neuron   = [Double] -> Double
type    Layer    = [Neuron]
type    Network  = [Layer]

tanhSigmoid x = tanh x
linearSigmoid x | x > 1.0 = 1.0
linearSigmoid x | x < 0.0 = 0.0
linearSigmoid x = x

sigmoid = linearSigmoid

neuron ws input idx | length ws == length input = {-trace ("neuron input:" ++ show input ++ " ws:" ++ show ws ++ " output: " ++ show out) $-} out
  where
    out = sigmoid $ sum $ map (\(w,i) -> w*i) (zip ws input)
neuron ws input idx {-| trace ("idx: " ++ show idx ++ " inp: " ++ show input ++ " ws: " ++ show ws) True -} = sigmoid ((input!!idx) * ws !! (0))

layer neurons input = {-trace ("layer input:" ++ show input ++ " out:" ++ show out) $-} out
  where
    out = map (\(f,idx) -> f input idx) (zip neurons [0..])

--network :: Layer -> [Double] -> [Double]
---- layer has to be partially applied with neurons
network layers input = {-trace ("network input:" ++ show input ++ " out:"++show out) $-} out
  where
    out = foldl (\input layer -> layer input) input layers

nestList []     _   = []
nestList (t:ts) lst = l : nestList ts ls
  where
    (l,ls) = splitAt t lst

createLayer ws = map (\x -> map neuron x) ws

topoToLayerCfg topo = topoToLayerCfg' topo'
  where
    topo' = 1 : topo
    topoToLayerCfg' (a:b:rest) = replicate b a : topoToLayerCfg' (b:rest)
    topoToLayerCfg' (_:[])     = []

layerCfgToNeuronCfg = map $ map $ flip replicate ()

fromCfgToLayer   cfg = layer $ map neuron cfg
fromCfgToNetwork cfg = network $ map fromCfgToLayer cfg

matingF t1 t2 = map (\(a,b)
                      -> map (\(u,v)
                              -> map (\(x,y) -> x + (y-x)/2) $ zip u v) $ zip a b) $ zip t1 t2

main = do
--  print $ nestList [2,3,1] ws
  randomizedCfg <- forM [cfg | _<-[1..100]] (traverse (traverse (traverse (\_ -> randomIO >>= (\x -> return $ (1-x*2)) :: IO Double))))
-- (traverse (traverse (traverse (\x -> randomIO :: IO Double))) cfg)
  let
    --randomizedCfg = [[[1],[1]],[[1,-1],[-1,1]],[[2,2]]]
    inputs = [[x,y] | x <- [0,1], y <- [0,1]]
    referenceOutput = [[0],[1],[1],[0]] in do
    --net = fromCfgToNetwork randomizedCfg in do
--      print randomizedCfg
--      print cfg
--      print inputs
      --print  $ map net inputs
      print $ runGen matingF fromCfgToNetwork 0.0001 randomizedCfg inputs referenceOutput ws 0
  where
    g   = mkStdGen 42
    ws  = randoms g :: [Double]
    cfg = layerCfgToNeuronCfg $ topoToLayerCfg [2,4,1]
