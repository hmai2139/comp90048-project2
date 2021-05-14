import Proj2
import Data.List
import Card 

main :: IO ()
main = 
    test [Card Club R2, Card Heart R7, Card Spade Ace]
    --test [Card Club R2, Card Spade Ace]
    
combs :: Int -> [a] -> [[a]]
combs n xs = let l = length xs
                          in if n>l then [] else combs xs !! (l-n)
 where
   combs [] = [[[]]]
   combs (x:xs) = let next = combs xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

combs2 0 _ = [[]]
combs2 _ [] = []
combs2 n (x:xs) = map (x:) (combs2 (n - 1) xs) ++ combs2 n xs

doubles = (combs 2 ([minBound..maxBound]::[Card]))
triples = (combs 3 ([minBound..maxBound]::[Card]))
quadruples = (combs 4 ([minBound..maxBound]::[Card]))

testAll [] = do return ()
testAll (x:xs) = 
    do test x
       testAll xs


test :: [Card] -> IO ()
test answer =
    do testProject answer 1 ([],[])

testProject :: [Card] -> Int -> ([Card], GameState) -> IO ()
testProject answer n prevGuessAndState =
    do
        if n == 1
        then
            do
                let guess = fst (initialGuess (length answer))
                let state = snd (initialGuess (length answer)) 
                putStrLn ("Your guess " ++ (show n) ++ ":  " ++ (show guess))
                putStrLn ("My answer:  " ++ show (feedback answer guess))
                if sort guess == sort answer
                then
                    do
                        putStrLn ("You got it in " ++ (show n) ++ " guess!")
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

                putStrLn ("Your guess " ++ (show n) ++ ":  " ++ (show guess))
                putStrLn ("My answer:  " ++ show (feedback answer guess))
                if sort guess == sort answer
                then
                    do 
                        putStrLn ("You got it in " ++ (show n) ++ " guess!")
                        return ()
                else
                    do
                        testProject answer (n+1) (guess, state)
                
            


        
