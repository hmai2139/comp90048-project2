import Proj2
import Data.List
import Card
import System.TimeIt

main :: IO ()
main = 
    --test [Card Club R2, Card Spade Ace, Card Diamond R10, Card Heart King, Card Club R8]
    --test [Card Diamond King, Card Heart R7]
    --test [Card Club R8, Card Heart R5, Card Diamond R4]
    --test [Card Club R2, Card Spade Ace]
    --test [Card Club King, Card Heart Queen]
    testAll doubles
    --testAll triples
    --testAll quadruples

guess :: [Card]
guess = [Card Club R2, Card Spade Ace, Card Diamond R7, Card Heart King]

combs :: Int -> [a] -> [[a]]
combs n xs = let l = length xs
                          in if n>l then [] else combs xs !! (l-n)
 where
   combs [] = [[[]]]
   combs (x:xs) = let next = combs xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

combs2 :: (Eq t, Num t) => t -> [a] -> [[a]]
combs2 0 _ = [[]]
combs2 _ [] = []
combs2 n (x:xs) = map (x:) (combs2 (n - 1) xs) ++ combs2 n xs

-- Group possible Answers to a given Guess by their feedback.
sorted :: [Card] -> [[Card]] -> [[Card]]
sorted g answers = sortOn (\ans -> feedback ans g) answers

grouped :: [Card] -> [[Card]] -> [[[Card]]]
grouped g answers = groupBy (\a1 a2 -> feedback a1 g == feedback a2 g) (sorted g answers)

-- Expected number of remaining possible Answers for a given Guess.
groupSizes :: [Card] -> [[Card]] -> [Int]
groupSizes g answers = map length (grouped g answers)

groupSizesSquared :: [Card] -> [[Card]] -> [Int]
groupSizesSquared g answers = map (^2) (groupSizes g answers)

avgAnsNum :: Fractional a => [Card] -> [[Card]] -> a
avgAnsNum g answers = fromIntegral (sum (groupSizesSquared g answers))
                    / fromIntegral (sum (groupSizes g answers))

-- All combinations of length 2-4 of Card.
doubles :: [[Card]]
doubles = combs 2 ([minBound..maxBound]::[Card])

triples :: [[Card]]
triples = combs 3 ([minBound..maxBound]::[Card])

quadruples :: [[Card]]
quadruples = combs 4 ([minBound..maxBound]::[Card])

quality :: Double -> Double
quality guessNum =
    min 1 $ max 0 $ 1.25 - log guessNum / (4 * log 4)

testAll :: [[Card]] -> IO ()
testAll [] = do return ()
testAll (x:xs) =
    do timeIt $ test x
       testAll xs

test :: [Card] -> IO ()
test answer =
    do timeIt $ testProject answer 1 ([],[])

testProject :: [Card] -> Int -> ([Card], GameState) -> IO ()
testProject answer n prevGuessAndState =
    do
        if n == 1
        then
            do  
                putStrLn "------------------------------"
                putStrLn ("Test " ++ show answer)
                let guess = fst (initialGuess (length answer))
                let state = snd (initialGuess (length answer))
                putStrLn ("Your guess " ++ show n ++ ":  " ++ show guess)
                putStrLn ("My answer:  " ++ show (feedback answer guess))
                if sort guess == sort answer
                then
                    do
                        putStrLn ("You got it in " ++ show n ++ " guess!")
                        let q = 100 * (quality $ (fromIntegral n))                       
                        putStrLn ("Approximate quality = " ++ show q ++ "%")
                        putStrLn "------------------------------"
                        return ()
                else
                    do
                        testProject answer (n+1) (guess, state)
        else
            do
                -- Previous guess and state.
                let prevGuess = fst prevGuessAndState
                let prevState = snd prevGuessAndState
                let prevFeedback = feedback answer prevGuess

                -- New guess and state.
                let newGuessAndState = nextGuess prevGuessAndState prevFeedback
                let guess = fst newGuessAndState
                let state = snd newGuessAndState

                putStrLn ("Your guess " ++ show n ++ ":  " ++ show guess)
                putStrLn ("My answer:  " ++ show (feedback answer guess))
                if sort guess == sort answer
                then
                    do
                        putStrLn ("You got it in " ++ show n ++ " guess!")
                        let q = 100 * (quality $ (fromIntegral n))                       
                        putStrLn ("Approximate quality = " ++ show q ++ "%")
                        putStrLn "------------------------------"
                        return ()
                else
                    do
                        testProject answer (n+1) (guess, state)





