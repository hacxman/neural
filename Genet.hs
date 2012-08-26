module Genet where

import Data.List
import Debug.Trace
import Control.Parallel.Strategies

objectiveFun reference test = (**2) $ sum $ map (\(x,y) -> {-trace ("x:"++show (x!!0) ++ " y:"++show (y!!0)) $ -} abs $ (x!!0) - (y!!0)) (zip reference test)


type Individuum = [[[Double]]]
type Input      = [Double]
type Output     = [Double]
runPopulation :: (Individuum -> Input -> Output) 
              -> [Input] -> [Individuum] -> [[Output]]
runPopulation fn inputs population =
    map (\individual -> map (fn individual) inputs) population
--map (uncurry fn) $ zip population input

testPopulation reference results = map (objectiveFun reference) results

-- errors are results from testPopulation
selectPop population errors = take 10 $ sortBy (\(x,_) (y,_) -> compare x y) $ zip errors population

crossPopulation :: (a -> a -> a) -> [a] -> [a]
crossPopulation matingF population = [matingF a b | a <- population, b <- population]

--mutatePopulation :: MutatingF -> [Individuum] -> ([Individuum], [Double])
mutateIndividuum individuum rands = level1 individuum rands
  where
    level3 items rnd = (zipWith (\x y -> x+(y*2-1)/10000.0) items $ take (length items) rnd
                       ,drop (length items) rnd)
    level2 items rnd = foldl (\(i,rs) y -> let (x',y') = level3 y rs in (x':i,y')) ([], rnd) items
    level1 items rnd = foldl (\(i,rs) y -> let (x',y') = level2 y rs in (x':i,y')) ([], rnd) items

mutatePopulation population rands = foldl (\(i,rs) y -> let (x',y') = mutateIndividuum y rs in (x':i,y')) ([], rands) population


runGen :: (Individuum -> Individuum -> Individuum)
       -> (Individuum -> Input -> Output)
       -> Double -> [Individuum] -> [Input] -> [Output] -> [Double] -> Int -> (Double, Individuum)
runGen matingF fn maxerror population inputs reference rands iii = let
  pop  = runPopulation fn inputs population
  test = testPopulation reference pop
  sele = selectPop population test
  npop = crossPopulation matingF $ (parMap rpar (uncurry (flip const)) sele)
  err  = fst (head sele)
  (mpop, nrands) = mutatePopulation npop rands
  in
    if iii == 10 then error ("SELE:" ++ show sele ++ "\nTEST:" ++ show test)
    else
      if err < maxerror
        then trace (show err) $ head sele
        else
          trace "pllm:" $
          trace (show err) $
          trace ("sele:" ++ show sele) $
          trace ("test:" ++ show test ++ "\n") $
          runGen matingF fn maxerror mpop inputs reference nrands (iii+1)
