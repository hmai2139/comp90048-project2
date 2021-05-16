## COMP90048 Declarative Programming SM1 2021 - Project 2 - Card Guessing Game
<p> The objective of this project is to practice and assess your understanding of functional programming and Haskell. You will write code to implement a logical deduction game.
The Game

Two players face each other, each with a complete standard deck of western playing cards (without jokers). One player will be the answerer and the other is the guesser. The answerer begins by selecting some number of cards from his or her deck without showing the guesser. These cards will form the answer for this game. The aim of the game is for the guesser to guess the answer.

Once the answerer has selected the answer, the guesser chooses the same number of cards from his or her deck to form the guess and shows them to the answerer. The answerer responds by telling the guesser these five numbers as feedback for the guess:

1. How many of the cards in the answer are also in the guess (correct cards).`

2. How many cards in the answer have rank lower than the lowest rank in the guess (lower ranks). Ranks, in order from low to high, are 2–10, Jack, Queen, King, and Ace.

3. How many of the cards in the answer have the same rank as a card in the guess (correct ranks). For this, each card in the guess is only counted once. That is, if the answer has two queens and the guess has one, the correct ranks number would be 1, not 2. Likewise if there is one queen in the answer and two in the guess.

4. How many cards in the answer have rank higher than the highest rank in the guess (higher ranks).

5. How many of the cards in the answer have the same suit as a card in the guess, only counting a card in the guess once (correct suits). For example, if the answer has two clubs and the guess has one club, or vice versa, the correct suits number would be 1, not 2.

Note that the order of the cards in the answer and the guess is immaterial, and that, since they come from a single deck, cards cannot be repeated in either answer or guess.

The guesser then guesses again, and receives feedback for the new guess, repeating the process until the guesser guesses the answer correctly. The object of the game for the guesser is to guess the answer with the fewest possible guesses.

A few examples of the feedback for a given answer and guess (with clubs, diamonds, hearts, and spades shown as ♣, ♦, ♥, and ♠, respectively):
| Answer | Guess | Feedback  |
|--------|-------|-----------|
| 3♣,4♥  | 4♥,3♣ | 2,0,2,0,2 |
| 3♣,4♥  | 3♣,3♥;| 1,0,1,1,2 |
| 3♦,3♠  | 3♣,3♥ | 0,0,2,0,0 |
| 3♣,4♥  | 2♥,3♥ | 0,0,1,1,1 |
| A♣,2♣  | 3♣,4♥ | 0,1,0,1,1 |

## The Program

For this assignment, you will write Haskell code to implement code for both the answerer and guesser parts of the game. This will require you to write a function to evaluate a guess to produce feedback, one to provide an initial guess, and one to use the feedback from the previous guess to determine the next guess. The latter function will be called repeatedly until it produces the correct guess. You will find it useful to keep information between guesses; since Haskell is a purely functional language, you cannot use a global or static variable to store this. Therefore, your initial guess function must return this game state information, and your next guess function must take the game state as input and return the updated game state as output. You may put any information you like in the game state, but you must define a type GameState to hold this information.

I will supply a Card module providing the Card, Rank, and Suit types and their constructors, similar to the types used in lectures. The suits are (in increasing order) Club, Diamond, Heart, and Spade, and the ranks are (in increasing order) R2, R3, R4, R5, R6, R7, R8, R9, R10, Jack, Queen, King, Ace, and cards are of the form Card suit rank.

The Card module has a few enhancements over the types presented in lectures. First, all three types are in the Eq and Ord classes. All are also in the Bounded and Enum classes, which means, for example, that [minBound..maxBound]::[Card] is the list of all cards in order, from 2 to A, and similarly [minBound..maxBound]::[Rank] is the list of ranks from 2 to Ace, and similarly for Suit. This also means that, for example, succ (Card Club R5) == (Card Club R6) and succ (Card Heart Ace) == (Card Spade R2). Read the documentation for Bounded and Enum classes for more things you can do with them.

For convenience, all three types are also in the Show class so that ranks and suits are shown as a single character (10 is shown as T), and cards as two characters: rank followed by suit. All three are also in the Read class, which means that, for example, (read "[2C,AH]")::[Card] returns the list [Card Club R2, Card Heart Ace] (which would be printed as [2C,AH]).

You must define the following three functions, as well as the GameState type:

### `feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)`
Takes a target and a guess (in that order), each represented as a list of Cards, and returns the five feedback numbers, as explained above, as a tuple.

### `initialGuess :: Int -> ([Card],GameState)`
Takes the number of cards in the answer as input and returns a pair of an initial guess, which should be a list of the specified number of cards, and a game state. The number of cards specified will be 2 for most of the test, and 3 or 4 for the remaining tests, as explained below.

### `nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)`
Takes as input a pair of the previous guess and game state, and the feedback to this guess as a quintuple of counts of correct cards, low ranks, correct ranks, high ranks, and correct suits, and returns a pair of the next guess and new game state.
<p>
