-- | COMP90048 S1 2021, Project 2 - Card Guessing Game
--   Author: Hoang Viet Mai
--   Student ID: 813361.
--
-- | This file implements an answerer and a guesser in a 
--   Card Guessing Game, whose rules are as follow:
--   The answerer picks a given number of cards from their deck
--   of western playing cards excluding Jokers i.e 52 unique cards.
--   For each round, the guesser chooses the same number of cards 
--   from their own deck to form a guess, then shows them to the answerer.
--   The answerer give the guesser a feedback of five numbers, whose details
--   are given in the `feedback` function below.
--   This process will be repeated until the guesser guesses the answer
--   correctly, thus ending the game. The guesser aims to produce the
--   correct answer with the fewest possible guesses.

module Proj2 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
import Data.Ord

-- | Represents a given game state, which is a List of 
--   possible answers, each of whom is a List of Cards.
type GameState = [[Card]]

-- | Takes a target and a guess, each represented as a List of Cards
--   of the same length, and returns a quintuple of Ints representing
--   a given feedback. A given feedback consists of the following:
--   1. Number of Cards appearing in both the answer and the guess.
--   2. Number of Cards in the answer whose ranks are lower than 
--      the lowest rank in the guess.
--   3. Number of Cards in the answer whose ranks are the same as 
--      the rank of a card in the guess. 
--   4. Number of Cards in the answer whose ranks are higher than
--      the highest rank in the guess.
--   5. Number of Cards in the answer whose suits are the same as 
--      the suit of a card in the guess.
--
-- | In order to avoid counting a card more than once:
--   For the counts of exact matches/lower ranks/higher ranks, 
--   the function assesses only one card in the answer for each 
--   recursive call, incrementing the counts as necessary.
--   For the count of same ranks, the function compute the list of ranks
--   in answer that are not in the guess, taking into account the number 
--   of occurences of a given rank. It then removes all such ranks from 
--   the list of ranks in the answer. The length of the final list -
--   consisting of only the ranks appearing in both the answer and the guess,
--   with their number of occurences taken into account - is thus
--   the required count. The same logic applies to the count of same suits.
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int) 
feedback [] guess = (0, 0, 0, 0, 0)
feedback (card:cards) guess =
    let (exact, lower, sameRank, higher, sameSuit) = feedback cards guess
    in ( exact + exactInc
       , lower + lowerInc
       , length (answerRanks \\ (answerRanks \\ guessRanks))
       , higher + higherInc
       , length (answerSuits \\ (answerSuits \\ guessSuits))
       )
    where 
        answerRanks = map rank (card:cards)
        guessRanks  = map rank guess
        answerSuits = map suit (card:cards)
        guessSuits  = map suit guess
        exactInc
            | card `elem` guess = 1
            | otherwise         = 0
        lowerInc
            | rank card < minimum guessRanks = 1
            | otherwise                      = 0
        higherInc
            | rank card > maximum guessRanks = 1
            | otherwise                      = 0

-- | Takes the number of cards in the answer as input and 
--   returns a pair of an initial guess, which is a list 
--   of the specified number of cards, and a game state.
--
-- | To produce the best initial guess, for an answer of `n` cards,
--   the function chooses cards with different suits that are equally
--   distant from each other and from the top and the bottom ranks, 
--   with the distance rounded down, to a minimum of 1-card distance.
--
-- | The minimum rank of the guess is the distance-th successor of the
--   bottom rank. Then 2nd rank is the distance-th successor of the minimum
--   rank, the 3rd rank is the distance-th successor of the 2nd rank, and
--   so on. The maximum rank of the guess is thus the distance-th predecessor
--   of the top rank. The minimum rank to maximum rank cycle is repeated
--   in an infinite list of ranks until all `n` ranks are chosen. 
--   The suits are also chosen from an infinite list of 4-suit cycle.
--   The first 4 ranks will be paired with Club, Diamond, Heart, Spade;
--   the same logic applies to the 5th-8th ranks, and to the 9th-12th ranks,
--   and so on until the nth rank.
--
-- | The list of possible answers will be a list of all combinations
--   of length `n` of all cards in the deck. The combination of cards
--   representing the initial guess will be removed at the start of 
--   next round, if they are not correct.
initialGuess :: Int -> ([Card], GameState)
initialGuess n = (guess, state)
    where guess = [Card suit rank | (suit, rank) <- zip suits ranks]
          distance = max 1 (13 `div` (n + 1))
          r1 = iterate succ minBound !! distance
          r2 = iterate succ r1 !! distance
          rn = iterate pred maxBound !! distance
          ranks
              | n < 13    = take n $ cycle $ enumFromThenTo r1 r2 rn :: [Rank]
              | otherwise = take n $ cycle $ [minBound..maxBound] :: [Rank]
          suits = take n $ cycle $ [minBound..maxBound] :: [Suit]
          state = combinations n ([minBound..maxBound] :: [Card])

-- | Takes as input a pair of the previous guess and game state, 
--   and the feedback to this guess as a quintuple of counts 
--   of correct cards, low ranks, correct ranks, high ranks, and
--   correct suits, and returns a pair of the next guess and new game state.

-- | This function filters out the possible answers that are inconsistent
--   with any answers to previous guessess. A given possible answer A
--   is inconsistent if the feedback F' the guesser would have received 
--   for a given guess G had A been the actual answer, is different
--   from the actual feedback F the guesser received by choosing G 
--   in the previous round.
--
-- | Using the implemented feedback function, this function removes
--   all inconsistent answers from the current game state. Then it simply
--   chooses the first answer from the filtered state. This process is
--   repeated until the correct answer is chosen.
nextGuess 
    :: ([Card], GameState)
    -> (Int, Int, Int, Int, Int)
    -> ([Card], GameState)
nextGuess (guess, state) (e, l, sr, h, ss) = (newGuess, newState)
    where 
        newGuess = head answers
        newState = tail answers
        answers  = delete guess $ filter (\ a -> feedback a guess == prev) state
        prev     = (e, l, sr, h, ss)
          
-- | List of all combinations of a given length of all elements of
--   a given List. Each combination contains no repeated elements.
combinations :: Int -> [a]-> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n - 1) xs) ++ combinations n xs
