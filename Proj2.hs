--{-# OPTIONS -Wall #-} 
-- Replace this comment with your opening documentation.  Leave this module declaration as is:
module Proj2 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List

{- | Represents a given game state, which is a List of possible answers,
     each of whom is a List of Cards.
-}
type GameState = [[Card]]
    
{- | Takes a target and a guess (in that order), each represented as 
     a list of Cards, and returns the five feedback numbers, as a quintuple.
-}
feedback 
    :: [Card]                     -- List of Cards representing a given answer.
    -> [Card]                     -- List of Cards representing a given guess.
    -> (Int, Int, Int, Int, Int)  -- Quintuple of Ints representing feedback.
feedback [] guess = (0, 0, 0, 0, 0)
feedback (card:cards) guess = 
    let (exact, lower, sameRank, higher, sameSuit) = feedback cards guess
    in ( exact + exactInc
       , lower + lowerInc
       , length (answerRanks \\ (answerRanks \\ guessRanks))
       , higher + higherInc
       , length (answerSuits \\ (answerSuits \\ guessSuits))
       ) 
    where answerRanks = map rank (card:cards)  
          guessRanks  = map rank guess         
          answerSuits = map suit (card:cards)  
          guessSuits  = map suit guess         
        
          exactInc  | card `elem` guess              = 1 
                    | otherwise                      = 0
          lowerInc  | rank card < minimum guessRanks = 1  
                    | otherwise                      = 0
          higherInc | rank card > maximum guessRanks = 1
                    | otherwise                      = 0
        
{- | Takes the number of cards in the answer as input and 
     returns a pair of an initial guess, 
     which is a list of the specified number of cards, and a game state.
-}
initialGuess 
    :: Int                  -- Number of cards to guess.
    -> ([Card], GameState)  -- Represents a guess.
initialGuess n = (cards, gameState)
    where cards = [Card s r | (s, r) <- zip suits ranks]
          distance = max 1 (13 `div` (n + 1))
          r1 = (iterate succ minBound) !! distance
          r2 = (iterate succ r1) !! distance
          rn = (iterate pred maxBound) !! distance
          ranks | n < 13    = take n (cycle (enumFromThenTo r1 r2 rn :: [Rank]))
                | otherwise = take n (cycle [minBound..maxBound] :: [Rank])
          suits = take n (cycle [minBound..maxBound] :: [Suit])     
          gameState = combinations n decks
          combinations n decks = filter ((n==).length) (subsequences decks)
          decks = [minBound..maxBound] :: [Card]


{- | Takes as input a pair of the previous guess and game state, 
     and the feedback to this guess as a quintuple of counts of correct cards, 
     low ranks, correct ranks, high ranks, and correct suits, 
     and returns a pair of the next guess and new game state.
-}
nextGuess 
    :: ([Card], GameState)        -- Represents the previous guess & game state.
    -> (Int, Int, Int, Int, Int)  -- Quintuple of Ints representing feedback.
    -> ([Card], GameState)        -- Represents the next guess.
nextGuess prevGuessAndState prevFeedback = (cards, newState)
    where cards = (filter (\ g -> avgAnsNum g == minAvgAnsNum) answers) !! 0
          --cards = answers !! 0
          newState = delete cards answers
          answers = filter (\ ans -> (feedback ans guess) == prevFeedback) state
          --answers = state
          guess = fst prevGuessAndState
          state = snd prevGuessAndState
          
          -- Group possible Answers to a given Guess by their feedback.
          groupedAns g = groupBy (\a1 a2 -> feedback a1 g == feedback a2 g) (sortedAns g)
          sortedAns g = sortOn (feedback g) answers
          
          -- Expected number of remaining possible Answers for a given Guess.
          groupSizes g = map length (groupedAns g)
          groupSizesSquared g = map (^2) (groupSizes g)
          avgAnsNum g = fromIntegral (sum (groupSizesSquared g))
                      / fromIntegral (sum (groupSizes g))
          
          -- Find the minimum expected number of possible Answers.
          avgAnsNums = map avgAnsNum answers
          minAvgAnsNum = minimum avgAnsNums
           
--sortOn (feedback [Card Club R3, Card Heart R4]) [[Card Club R3, Card Heart R4] , [Card Club R3, Card Heart R3], [Card Club R3, Card Heart R4], [Card Heart R3, Card Club R3]]
--groupBy (\x y-> feedback x [Card Club R3, Card Heart R4] ==feedback y [Card Club R3, Card Heart R4]) (sortOn (feedback [Card Club R3, Card Heart R4]) [[Card Club R3, Card Heart R4] , [Card Club R3, Card Heart R3], [Card Club R3, Card Heart R4], [Card Heart R3, Card Club R3]])