{-# LANGUAGE CPP #-}

module PolyCalculator (repl, evaluate) where

#ifdef GRADESCOPE
import           AutograderUtils (getChar, getLine, print, putStrLn, readLn)
import           Prelude         hiding (getChar, getLine, print, putStrLn,
                                  readLn)
#endif
import           Parser          (Polynomial (..), expand, parse, polynomial,
                                  simplify)

parseFailed, polynomialLoaded, noPolynomialLoaded, polynomialSaved, noPolynomialMemorised, unbearable :: String
parseFailed = "Could not parse the polynomial."
polynomialLoaded = "Polynomial loaded."
noPolynomialLoaded = "No polynomial loaded."
polynomialSaved = "Polynomial saved."
noPolynomialMemorised = "No polynomial memorised."
unbearable = "Unbearable action!"

repl :: IO ()
repl = program Nothing Nothing

type MP = Maybe Polynomial -- im lazy

program :: MP -> MP -> IO ()
program curr mem = do
    str <- getLine
    case words str of
        "load" : args   -> load curr mem (concat args)
        ["evaluate", n] -> evaluateP curr mem (read n)
        ["memorise"]    -> memorise curr mem
        ["recall"]      -> recall curr mem
        ["clear"]       -> program curr Nothing
        ["reset"]       -> program Nothing Nothing
        ["expand"]      -> expandP curr mem
        ["simplify"]    -> simplifyP curr mem
        ["quit"]        -> pure ()
        _badArguments   -> putStrLn unbearable >> program curr mem


load :: MP -> MP -> String -> IO ()
load curr mem poly = case parseResult of
    Nothing -> putStrLn parseFailed >> program curr mem
    Just p  -> putStrLn polynomialLoaded >> program (Just p) mem
    where parseResult = do
            (ply, rest) <- parse polynomial (dropWhile (== ' ') poly)
            if all (== ' ') rest then pure ply else Nothing


evaluateP :: MP -> MP -> Integer -> IO ()
evaluateP curr mem n = do
    case curr of
        Nothing -> putStrLn noPolynomialLoaded
        Just p  ->  print $ evaluate p n
    program curr mem


memorise :: MP -> MP -> IO ()
memorise curr mem = case curr of
    Nothing -> putStrLn noPolynomialLoaded >> program curr mem
    _       -> putStrLn polynomialSaved >> program curr curr


recall :: MP -> MP -> IO ()
recall curr mem = case mem of
    Nothing -> putStrLn noPolynomialMemorised >> program curr mem
    Just p  ->  print p >> program mem mem


expandP :: MP -> MP -> IO ()
expandP curr mem = case curr of
    Nothing -> putStrLn noPolynomialLoaded >> program curr mem
    Just p  -> let p' = expand p in print p' >> program (Just p') mem


simplifyP :: MP -> MP -> IO ()
simplifyP curr mem = case curr of
    Nothing -> putStrLn noPolynomialLoaded >> program curr mem
    Just p  -> let p' = simplify p in print p' >> program (Just p') mem


evaluate :: Polynomial -> Integer -> Integer
evaluate (Mono c d) k = c*(k^d)
evaluate (Add p q) k = evaluate p k + evaluate q k
evaluate (Mul p q) k = evaluate p k * evaluate q k
