-- Replace this comment with your opening documentation.  Leave this module declaration as is:
module Proj2 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
import Data.Ord

{- | Represents a given game state, which is a List of possible answers,
     each of whom is a List of Cards.
-}
type GameState = [[Card]]

{- | Takes a target and a guess, each represented as a List of Cards
     of the same length, and returns a quintuple of Ints representing
     a given feedback. A given feedback consists of the following:
   - Number of Cards appearing in both the answer and the guess.
   - Number of Cards in the answer whose ranks are lower than the lowest
     rank in the guess.
   - Number of Cards in the answer whose ranks are the same as the rank of 
     a card in the guess. 
     Counting a card in the guess only once.
   - Number of Cards in the answer whose ranks are higher than the highest
     rank in the guess.
   - Number of Cards in the answer whose suits are the same as the suit of 
     a card in the guess.
     Counting a card in the guess only once.
-}
feedback
    :: [Card]                     -- A given answer.
    -> [Card]                     -- A given guess.
    -> (Int, Int, Int, Int, Int)  -- A given feedback.
feedback [] guess = (0, 0, 0, 0, 0)
feedback (card:cards) guess =
    let (e, l, sr, h, ss) = feedback cards guess
    in ( e + eInc
       , l + lInc
       , length (ar \\ (ar \\ gr))
       , h + hInc
       , length (as \\ (as \\ gs))
       )
    where ar = map rank (card:cards)
          gr = map rank guess
          as = map suit (card:cards)
          gs = map suit guess
          eInc
              | card `elem` guess      = 1
              | otherwise              = 0
          lInc
              | rank card < minimum gr = 1
              | otherwise              = 0
          hInc
              | rank card > maximum gr = 1
              | otherwise              = 0

{- | Takes the number of cards in the answer as input and 
     returns a pair of an initial guess, 
     which is a list of the specified number of cards, and a game state.
-}
initialGuess
    :: Int                  -- Number of cards to guess.
    -> ([Card], GameState)  -- A given guess.
initialGuess n = (guess, state)
    where guess = [Card suit rank | (suit, rank) <- zip suits ranks]
          distance = max 1 (13 `div` (n + 1))
          r1 = iterate succ minBound !! distance
          r2 = iterate succ r1 !! dist
          rn = iterate pred maxBound !! distance
          ranks
              | n < 13    = take n (cycle (enumFromThenTo r1 r2 rn::[Rank]))
              | otherwise = take n (cycle [minBound..maxBound]::[Rank])
          suits = take n (cycle [minBound..maxBound]::[Suit])
          state = []

{- | Takes as input a pair of the previous guess and game state, 
     and the feedback to this guess as a quintuple of counts of correct cards, 
     low ranks, correct ranks, high ranks, and correct suits, 
     and returns a pair of the next guess and new game state.
-}
nextGuess
    :: ([Card], GameState)        -- The previous guess & game state.
    -> (Int, Int, Int, Int, Int)  -- The previous feedback.
    -> ([Card], GameState)        -- The next guess.
nextGuess (guess, state) (e, l, sr, h, ss) = (newGuess, newState)
    where --newGuess = head sortedAns
          newGuess = head answers
          --newState = tail sortedAns
          newState = tail answers
          --sortedAns = sortOn avgAnsNum answers
          answers
              | state /= [] = filter (\ a -> feedback a guess == prev) state
              | otherwise   = filter (\ a -> feedback a guess == prev) (combs (length guess) deck)
          prev = (e, l, sr, h, ss)
          deck = [minBound..maxBound]::[Card]
          -- All combinations of length `n` of a given list.
          combs 0 _ = [[]]
          combs _ [] = []
          combs n (x:xs) = map (x:) (combs (n - 1) xs) ++ combs n xs

          -- Group possible Answers to a given Guess by their feedback.
          sorted g = sortOn (\ a -> feedback a g) answers
          grouped g = groupBy (\ a1 a2 -> feedback a1 g == feedback a2 g) (sorted g)

          -- Expected number of remaining possible Answers for a given Guess.
          groupSizes g = map length (grouped g)
          groupSizesSquared g = map (^2) (groupSizes g)
          avgAnsNum g = fromIntegral (sum (groupSizesSquared g))
                      / fromIntegral (sum (groupSizes g))
