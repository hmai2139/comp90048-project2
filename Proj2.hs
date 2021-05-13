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
          exactInc 
              | card `elem` guess              = 1 
              | otherwise                      = 0
          lowerInc
              | rank card < minimum guessRanks = 1  
              | otherwise                      = 0
          higherInc
              | rank card > maximum guessRanks = 1
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
          dist = max 1 (13 `div` (n + 1))
          r1 = (iterate succ minBound) !! dist
          r2 = (iterate succ r1) !! dist
          rn = (iterate pred maxBound) !! dist
          ranks
              | n < 13    = take n (cycle (enumFromThenTo r1 r2 rn :: [Rank]))
              | otherwise = take n (cycle [minBound..maxBound] :: [Rank])
          suits = take n (cycle [minBound..maxBound] :: [Suit])     
          gameState = []
 

{- | Takes as input a pair of the previous guess and game state, 
     and the feedback to this guess as a quintuple of counts of correct cards, 
     low ranks, correct ranks, high ranks, and correct suits, 
     and returns a pair of the next guess and new game state.
-}
nextGuess 
    :: ([Card], GameState)        -- Represents the previous guess & game state.
    -> (Int, Int, Int, Int, Int)  -- Quintuple of Ints representing feedback.
    -> ([Card], GameState)        -- Represents the next guess.
nextGuess (guess, state) prevFeedback = (cards, newState)
    where cards = head (sortOn (\ g -> avgAnsNum g) answers)
          newState = delete cards answers
          answers
              | state /= [] = filter (\ a -> (feedback a guess) == prevFeedback) state
              | otherwise   = filter (\ a -> (feedback a guess) == prevFeedback) (combs (length guess) deck)
          deck = [minBound..maxBound] :: [Card]
          
          -- All combinations of length `n` of a given list.
          combs 0 _ = [[]]
          combs _ [] = []
          combs n (x:xs) = map (x:) (combs (n - 1) xs) ++ combs n xs
                    
          -- Group possible Answers to a given Guess by their feedback.
          sorted g = sortOn (\ans -> feedback ans g) answers
          grouped g = groupBy (\a1 a2 -> feedback a1 g == feedback a2 g) (sorted g)
                   
          -- Expected number of remaining possible Answers for a given Guess.
          groupSizes g = map length (grouped g)
          groupSizesSquared g = map (^2) (groupSizes g)
          avgAnsNum g = fromIntegral (sum (groupSizesSquared g))
                      / fromIntegral (sum (groupSizes g))