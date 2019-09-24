import AcceptableTests
import AuxiliaryTests
import CompletionTests
import CPLTests
import LogicProgramsTests
import Test.Hspec
import Test.QuickCheck

    
main :: IO ()
main = do
    putStrLn "Tests for 'Accepatble' module"
    acceptableTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'Auxiliary' module"
    auxiliaryTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'Completion' module"
    completionTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'CPL' module"
    cplTests
    putStrLn ""
    putStrLn ""
    putStrLn "Tests for 'LogicPrograms' module"
    lpTests
