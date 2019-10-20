{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import LogicPrograms
import Completion
import Acceptable
import TranslationTp
import NeuralNetworks
import NNdecoder
import LPsimplifier
import Web.Scotty               (scotty, post, body, text, ActionM, liftAndCatchIO)
import Data.Text.Lazy           (Text, pack, unpack)
import Data.Text.Lazy.Encoding  (decodeUtf8)


main :: IO ()
main = scotty 10100 $ do

    post "/api/bP" $ do
        b <- decodeUtf8 <$> body
        responseLP b bp
    
    post "/api/positiveBodies" $ do
        b <- decodeUtf8 <$> body
        responseLP b lpPBodies
    
    post "/api/negativeBodies" $ do
        b <- decodeUtf8 <$> body
        responseLP b lpNBodies
    
    post "/api/completion" $ do
        b <- decodeUtf8 <$> body
        responseLP b comp
    
    post "/api/isAcceptable" $ do
        b <- decodeUtf8 <$> body
        responseLP b isAcceptable
    
    post "/api/getNN" $ do
        b <- decodeUtf8 <$> body
        responseNN b
    
    post "/api/abdAt" $ do
        b <- decodeUtf8 <$> body
        getNN_atomsAbd b
    
    post "/api/abdCl" $ do
        b <- decodeUtf8 <$> body
        getNN_clauseAbd b
    
    post "/api/nn2lp" $ do
        b <- decodeUtf8 <$> body
        nn2lp b


responseLP :: Show a => Text -> (LP -> a) -> Web.Scotty.ActionM ()
responseLP input function = textPackShow $ function $ readUnpackLP input
    where
        readUnpackLP x = read (unpack x) :: LP
        textPackShow x = text $ pack $ show x


responseString :: Text -> (String -> String) -> Web.Scotty.ActionM ()
responseString input function = textPackShow $ function $ readUnpackStr input
    where
        readUnpackStr x = unpack x
        textPackShow x  = text $ pack x


responseNN :: Text -> Web.Scotty.ActionM ()
responseNN input = do
    converted <- liftAndCatchIO nnPythonString
    text $ pack $ converted
    where
        inputLP        = read (unpack input) :: LP
        nnFactors      = NNfactors 1 1 0.05 0.0 0.5 0.5
        nnBase         = baseNN inputLP nnFactors
        nnAdd          = additionalNN nnBase nnFactors []
        nnFull         = do
            nn <- nnAdd
            return $ recursiveConnections nn (overlappingAtoms inputLP [])
        nnPythonString = do
            nn <- nnFull
            return $ nnToPythonString nn


getNN_atomsAbd :: Text -> Web.Scotty.ActionM ()
getNN_atomsAbd input = do
    converted <- liftAndCatchIO nnPythonString
    text $ pack $ converted
    where
        div     = lines $ unpack input
        inpLP   = read (div !! 0) :: LP
        abd     = read (div !! 1) :: [Atom]
        nnFac   = floatsToNNfac (read (div !! 2) :: [Float])
        nnBase  = baseNN inpLP nnFac
        nnAdd   = additionalNN nnBase nnFac abd
        nnFull  = do
            nn <- nnAdd
            return $ recursiveConnections nn (overlappingAtoms inpLP abd)
        nnPythonString = do
            nn <- nnFull
            return $ nnToPythonString nn


getNN_clauseAbd :: Text -> Web.Scotty.ActionM ()
getNN_clauseAbd input = do
    converted <- liftAndCatchIO nnPythonString
    text $ pack $ converted
    where
        div     = lines $ unpack input
        inpLP   = read (div !! 0) :: LP
        cl      = read (div !! 1) :: Clause
        abd     = [clHead cl]
        nnFac   = floatsToNNfac (read (div !! 2) :: [Float])
        modLP   = modifiedLP inpLP cl
        nnBase  = baseNN modLP nnFac
        nnAdd   = additionalNN nnBase nnFac abd
        nnFull  = do
            nn <- nnAdd
            return $ recursiveConnections nn (overlappingAtoms modLP abd)
        nnPythonString = do
            nn <- nnFull
            return $ nnToPythonString nn


floatsToNNfac :: [Float] -> NNfactors
floatsToNNfac (a:b:c:d:e:f:xs) = NNfactors
    { beta            = a
    , addHidNeuNumber = round b
    , addWeightLimit  = c
    , addNeuronsBias  = d
    , weightFactor    = e
    , aminFactor      = f
    }


nn2lp :: Text -> Web.Scotty.ActionM ()
nn2lp input = text $ pack $ show finalLP
    where
        div     = lines $ unpack input
        atsOrd  = (read (div !! 0) :: [Atom], read (div !! 1) :: [Atom])
        amin    = read (div !! 2) :: Float
        ioPairs = read (div !! 3) :: [([Int], [Float])]
        finalLP = simplifyLP $ decodeNN amin atsOrd ioPairs
