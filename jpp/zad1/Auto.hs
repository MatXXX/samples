module Auto (
    Auto,
    states,
    initStates,
    isAccepting,
    transition,
    accepts,
    emptyA,
    epsA,
    symA,
    leftA,
    sumA,
    thenA,
    toLists,
    fromLists
) where

import Data.List
data Auto a q = A {
    states :: [q],
    initStates :: [q],
    isAccepting :: q -> Bool,
    transition :: q -> a -> [q]
}

accepts :: Eq q => Auto a q -> [a] -> Bool
accepts auto word = acceptHelper word (nub $ initStates auto) where
    acceptHelper w s
        | null w    = any (isAccepting auto) s
        | otherwise = acceptHelper (tail w) (nextStates (head w) s)
        where
            nextStates l s = nub $ concat $ map (flip (transition auto) l) s

const2 :: a -> b -> c -> a
const2 a _ _ = a

emptyA :: Auto a ()
emptyA = A {
    states = [],
    initStates = [],
    isAccepting = const False,
    transition = const2 []
}

epsA :: Auto a ()
epsA = A {
    states = [()],
    initStates = [()],
    isAccepting = (==()),
    transition = const2 []
}

symA :: Eq a => a -> Auto a Bool
symA c = A {
    states = [True, False],
    initStates = [False],
    isAccepting = id,
    transition = \q a -> if q == False && a == c then [True] else []
}

leftA :: Auto a q -> Auto a (Either q r)
leftA aut = A {
    states = map Left $ states aut,
    initStates = map Left $ initStates aut,
    isAccepting = either (isAccepting aut) (const False),
    transition = \s l -> map Left $ either (transition aut) (const2 []) s l
}

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA a1 a2 = A {
    states = (map Left $ states a1) ++ (map Right $ states a2),
    initStates = (map Left $ initStates a1) ++ (map Right $ initStates a2),
    isAccepting = either (isAccepting a1) (isAccepting a2),
    transition = let leftTransition char l = map Left $ transition a1 l char
                     rightTransition char r = map Right $ transition a2 r char in
                 \state char -> either (leftTransition char) (rightTransition char) state
}

thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA a1 a2 = A {
    states = (map Left $ states a1) ++ (map Right $ states a2),
    initStates = let leftInits = map Left $ initStates a1
                     rightInits = (if any (isAccepting a1) (initStates a1)
                                    then map Right (initStates a2)
                                    else []) in
                 leftInits ++ rightInits,
    isAccepting = either (const False) (isAccepting a2),
    transition = let leftTransition char l = (map Left $ transition a1 l char) ++ 
                        if any (isAccepting a1) (transition a1 l char) 
                        then map Right $ initStates a2 
                        else []
                     rightTransition char r = map Right $ transition a2 r char in
                 \state char -> either (leftTransition char) (rightTransition char) state
}

getAlphabet :: (Enum a, Bounded a) => Auto a q -> [a]
getAlphabet _ = [minBound .. maxBound]

transitions :: (Enum a, Bounded a) => Auto a q -> [(q, a, [q])]
transitions aut = filter filterEmpty $ map createTranstion stateCharPairs where
    filterEmpty (_, _, c) = not $ null c 
    createTranstion (x, y) = (x, y, (transition aut) x y)
    stateCharPairs = cartesianProduct (states aut) (getAlphabet aut)
    cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

toLists :: (Enum a,Bounded a) => Auto a q -> ([q], [q], [q], [(q, a, [q])])
toLists aut =
    (states aut,
     initStates aut,
     filter (isAccepting aut) (states aut),
     transitions aut)

fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
fromLists stat initStat acceptingStat transitionsList = A {
    states = stat,
    initStates = initStat,
    isAccepting = (flip elem) acceptingStat,
    transition = let extractList (_, _, c) = c
                     matchTransitions q a (x, y, _) = x == q && y == a
                     matchingTransitions q a = filter (matchTransitions q a) transitionsList in
                 \q a -> concat $ map extractList $ matchingTransitions q a
}

instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
    show aut = intercalate " " $ "fromLists:" :
                   map show [states aut, initStates aut, filter (isAccepting aut) (states aut)]
                   ++ [show $ transitions aut]