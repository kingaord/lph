{-# LANGUAGE ParallelListComp #-}

{-|
Module      : TranslationPhi
Description : Functions that allow to translate a logic program into a neural
              network.
Copyright   : (c) Kinga O., 2020
License     : GPL-3length
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module TranslationPhi
    ( ) where

import Auxiliary
import NeuralNetworks
import LogicPrograms
import Data.List (length, maximum, map, find, (\\), delete, partition, foldl1)
import Data.Char 
import System.Random
import TranslationTp


inpOutRecNN :: LP -> Float -> NeuralNetwork
inpOutRecNN lp w = NN
    { inpLayer            = createInpLayer ++ 
                            [Neuron "Top" "threshold" 0 "inpTop", Neuron "Bot" "threshold" 0 "inpBot"]
    , hidLayer            = []
    , outLayer            = createOutLayer
    , recLayer            = []
    , inpToHidConnections = []
    , hidToOutConnections = []
    , recConnections      = createRecConns
    }
    where
        -- Herbrand base
        bp = LogicPrograms.bp lp

        -- Creates an input layer without neurons 'Top' (always true) and 'Bot'
        -- (always false); threshold for these neurons should be set to 0.5.
        createInpLayer = concat [ 
            [Neuron (show a ++ "Top") "threshold" 0 ("inp" ++ show idxTop), 
             Neuron (show a ++ "Bot") "threshold" 0 ("inp" ++ show ((+1) idxTop))] 
             | a      <- bp 
             | idxTop <- [1, 3 .. (length bp) * 2] ]

        -- Creates an output layer. Threshold for neurons marked as 'Top' should 
        -- be set to w/2; threshold for neurons marked as 'Bot' should be set to 
        -- max(w/2; l * w - w/2), where l is a number of clauses in logic program 
        -- with the same head.
        createOutLayer = concat [ 
            [Neuron (show a ++ "Top") "threshold" 0 ("out" ++ show idxTop), 
             Neuron (show a ++ "Bot") "threshold" 0 ("out" ++ show ((+1) idxTop))] 
             | a      <- bp 
             | idxTop <- [1, 3 .. (length bp) * 2] ]

        -- Creates recursive connections between an output and input layer. 
        createRecConns = [ Connection (NeuralNetworks.idx from) (NeuralNetworks.idx to) w 
             | from <- createOutLayer 
             | to   <- createInpLayer ]


updateHidLayer :: NeuralNetwork -> Clause -> Float -> Int -> NNupdate
updateHidLayer (NN inpL hidL outL recL ihC hoC recC) cl w index = case cl of

    Fact hd -> NNupdate
        { inpNeuToAdd      = [] 
        , hidNeuToAdd      = [ Neuron labelHidTop "threshold" 0 idxHidTop,
                               Neuron labelHidBot "threshold" 0 idxHidBot ]
        , outNeuToAdd      = []
        , outNeuToRemove   = []
        , inpToHidConToAdd = [ Connection "inpTop" idxHidTop w ]
        , hidToOutConToAdd = [ Connection idxHidTop (findNeuOut hd "Top") w, 
                               Connection idxHidBot (findNeuOut hd "Bot") w ]
        }

    Assumption hd -> NNupdate
        { inpNeuToAdd      = [] 
        , hidNeuToAdd      = [ Neuron labelHidTop "threshold" 0 idxHidTop,
                               Neuron labelHidBot "threshold" 0 idxHidBot ]
        , outNeuToAdd      = []
        , outNeuToRemove   = []
        , inpToHidConToAdd = [ Connection "inpBot" idxHidBot w ]
        , hidToOutConToAdd = [ Connection idxHidTop (findNeuOut hd "Top") w, 
                               Connection idxHidBot (findNeuOut hd "Bot") w ]
        }

    Cl hd bodyPos bodyNeg -> NNupdate
        { inpNeuToAdd      = [] 
        , hidNeuToAdd      = [ Neuron labelHidTop "threshold" 0 idxHidTop,
                               Neuron labelHidBot "threshold" 0 idxHidBot ]
        , outNeuToAdd      = []
        , outNeuToRemove   = []
        , inpToHidConToAdd = concat [ 
            [ Connection (findNeuInp atomPos "Top") idxHidTop w, 
              Connection (findNeuInp atomPos "Bot") idxHidBot w, 
              Connection (findNeuInp atomNeg "Top") idxHidBot w, 
              Connection (findNeuInp atomNeg "Bot") idxHidTop w ] 
              | atomPos <- bodyPos 
              | atomNeg <- bodyNeg ]
        , hidToOutConToAdd = [ Connection idxHidTop (findNeuOut hd "Top") w, 
                               Connection idxHidBot (findNeuOut hd "Bot") w ]
        }            

    where 
        nn = NN inpL hidL outL recL ihC hoC recC
        labelHidTop = "h" ++ show index ++ "Top"
        labelHidBot = "h" ++ show index ++ "Bot"
        idxHidTop   = "hid" ++ show index
        idxHidBot   = "hid" ++ show ((+1) index)
        findNeuInp  = \x y -> head [ NeuralNetworks.idx neu | neu <- inpL, NeuralNetworks.label neu == (show x) ++ y ] 
        findNeuOut  = \x y -> head [ NeuralNetworks.idx neu | neu <- outL, NeuralNetworks.label neu == (show x) ++ y ] 


fullNN :: LP -> Float -> NeuralNetwork -> NeuralNetwork
fullNN [] w nn     = nn
fullNN (c:cs) w nn = fullNN cs w (mergeNNupd nn (updateHidLayer nn c w index))
    where
        index = (+1) (length (NeuralNetworks.hidLayer nn))

p1 :: LP
p1 = [Cl (A 1 "") [A 2 ""] [A 3 ""], Assumption (A 3 "")]

p2 :: LP
p2 = [Cl (A 1 "") [A 2 ""] [A 3 ""], Assumption (A 3 ""), Cl (A 1 "") [A 4 ""] [A 5 ""], Assumption (A 5 "")]

p5 :: LP
p5 = [Cl (A 1 "") [A 2 ""] [A 3 ""], Assumption (A 3 ""), Fact (A 2 "")]