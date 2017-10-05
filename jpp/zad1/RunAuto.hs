import Auto
import System.Environment
import System.Exit
import System.IO
import Text.Read
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad

type Transition = (Integer, Alpha, [Integer])

parseTransition :: [String] -> Maybe [Transition]
parseTransition (antecedent:chars:consequents) = do
    ante <- readMaybe antecedent :: Maybe Integer
    str <- readMaybeAlpha chars :: Maybe [Alpha]
    conse <- sequence $ map (readMaybe :: String -> Maybe Integer) consequents
    return [(ante, c, conse) | c <- str]
parseTransition _ = Nothing

parseAuto :: [String] -> Maybe String
parseAuto lines = do
    statesCount <- readMaybe =<< listToMaybe lines 
    inits <- readMaybe =<< (listToMaybe $ tail lines)
    accept <- readMaybe =<< (listToMaybe $ tail $ tail lines)
    let transitionLines = map words $ drop 3 (init lines) in do
        transitions <- concat <$> (sequence $ map parseTransition transitionLines)
        lastLine <- readMaybeAlpha $ last lines
        let auto = fromLists [1..statesCount] inits accept transitions in
            Just (show $ accepts auto lastLine)

putLnMaybe :: Maybe String -> IO ()
putLnMaybe Nothing = putStrLn "BAD INPUT"
putLnMaybe (Just s) = putStrLn s

processFile :: String -> IO ()
processFile file = withFile file ReadMode $ \handle -> do
    res <- parseAuto <$> (filterEmpty <$> getLines handle)
    putLnMaybe res where
        filterEmpty = filter $ not . null
        getLines = (fmap lines) . hGetContents

parseArgs :: IO String
parseArgs = do
    args <- getArgs
    if length args /= 1 then do
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " <filename>"
        exitFailure
    else return $ args !! 0

main :: IO ()
main = parseArgs >>= processFile


newtype Alpha = Alpha Char deriving (Eq)

instance Bounded Alpha where
    minBound = Alpha 'A'
    maxBound = Alpha 'Z'

instance Enum Alpha where
    toEnum i = Alpha (['A'..'Z'] !! i)
    fromEnum a =
        case elemIndex a (map Alpha ['A'..'Z']) of
            Just i -> i
            Nothing -> error "Invalid argument"

instance Show Alpha where
    show a = show (['A'..'Z'] !! (fromEnum a))

readMaybeAlpha :: String -> Maybe [Alpha]
readMaybeAlpha str = sequence $ map readHelper str where
    readHelper x = 
        if isCorrectAlpha x then 
            Just (toEnum (ord x - ord 'A') :: Alpha)
        else Nothing
    isCorrectAlpha = flip elem $ ['A'..'Z']
