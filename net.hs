module Net where
import Random
import Data.Traversable

type    Neuron   = [Double] -> Double
type    Layer    = [Neuron]
type    Network  = [Layer]

tanhSigmoid x = tanh x
linearSigmoid x | x > 1.0 = 1.0
linearSigmoid x | x < 0.0 = 0.0
linearSigmoid x = x

sigmoid = linearSigmoid

neuron ws input = sigmoid $ sum $ map (\(w,i) -> w*i) (zip ws input)

layer neurons input = map (\f -> f input) neurons

--network :: Layer -> [Double] -> [Double]
---- layer has to be partially applied with neurons
network layers input = foldl (\input layer -> layer input) input layers

nestList []     _   = []
nestList (t:ts) lst = l : nestList ts ls
  where
    (l,ls) = splitAt t lst

createLayer ws = map (\x -> map neuron x) ws

topoToLayerCfg topo = topoToLayerCfg' topo'
  where
    topo' = 1 : topo
    topoToLayerCfg' (a:b:rest) = replicate b a : topoToLayerCfg' (b:rest)
    topoToLayerCfg' (_ : [])   = []

layerCfgToNeuronCfg = map $ map $ flip replicate ()

fromCfgToLayer   cfg = layer $ map neuron cfg
fromCfgToNetwork cfg = network $ map fromCfgToLayer cfg

main = do
--  print $ nestList [2,3,1] ws
  --randomizedCfg <- traverse (traverse (traverse (\x -> randomIO :: IO Double))) cfg
  let
    randomizedCfg = [[[1],[1]],[[1,-1],[-1,1]],[[2,2]]]
    net = fromCfgToNetwork randomizedCfg in
    print $ map net [[x,y] | x <- [0,1], y <- [0,1]]
  where
    g   = mkStdGen 42
    ws  = randoms g :: [Double]
    cfg = layerCfgToNeuronCfg $ topoToLayerCfg [2,2,1]
