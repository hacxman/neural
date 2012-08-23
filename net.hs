module Net where
import Random
import Data.Traversable

sigmoid x = tanh x --if x < 0 then 0 else 1

neuron ws input = sigmoid $ sum $ map (\(w,i) -> w*i) (zip ws input)

layer neurons input = map (\f -> f input) neurons

inputs = [1.0..5.0]

-- layer has to be partially applied with neurons
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

{-pllm (c:cfg) = pllm c

traverseList cfg rands = foldl (foldl ) ([], []) cfg
  where
    (rs,rest) = splitAt (length ws) rands
    mapInnermost ws = (map (\(w,r) -> r) $ zip ws rs, rest)
-}

main = do
--  print $ nestList [2,3,1] ws
  randomizedCfg <- traverse (traverse (traverse (\x -> randomIO :: IO Double))) cfg
  print 3 -- $ map (neuron) randomizedCfg
  where
    g   = mkStdGen 42
    ws  = randoms g :: [Double]
    cfg = layerCfgToNeuronCfg $ topoToLayerCfg [2,3,2]
