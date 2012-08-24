module Genet where

import Data.List

objectiveFun reference test = (/2) $ (**2) $ sum $ map (uncurry (-)) (zip reference test)

runPopulation fn input population = map (uncurry fn) $ zip population input

testPopulation reference results = map (objectiveFun reference) results

-- errors are results from testPopulation
selectPop population errors = take 10 $ sortBy (\(x,_) (y,_)-> compare x y) $ zip errors population

crossPopulation matingF population = [matingF a b | a <- population, b <- population]

runGen matingF fn maxerror population input reference = let
  pop  = runPopulation fn input population
  test = testPopulation reference pop
  sele = selectPop population test
  npop = crossPopulation matingF $ map (uncurry const) sele
  in
    if fst (head sele) < maxerror
      then head sele
      else runGen matingF fn maxerror npop input reference
