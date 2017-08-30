-- Author: Mingyang Zhang
-- Login: mingyangz
-- Creation date: 8/27/2017
-- Last modified: 8/28/2017

module Proj1 (initialGuess, nextGuess, allPitches, allChords, GameState) where


import qualified Data.Set as Set
import Data.List


data Note =
    A | B | C | D | E | F | G
    deriving (Eq, Ord, Show, Enum)


data Pitch =
    Pitch {note::Note, octave::Int}
    deriving Eq

instance Ord Pitch where
    compare (Pitch n1 o1) (Pitch n2 o2) =
        let porder = compare n1 n2
        in if porder == EQ then compare o1 o2 else porder

instance Show Pitch where
    show (Pitch note octave) = show note ++ show octave


-- Set of 3 Pitches
type Chord = Set.Set Pitch


-- List of potential Chords
type GameState = [Chord]


-- List of all valid Pitches
allPitches :: [Pitch]
allPitches = [Pitch note octave | note <- [A, B, C, D, E, F, G], octave <- [1, 2, 3]]


-- List of all valid Chords
allChords :: [Chord]
allChords = [Set.fromList [p1, p2, p3] | p1 <- allPitches, p2 <- allPitches, p3 <- allPitches, p1 < p2 && p2 < p3]


-- Convert Char to Note
charToNote :: Char -> Note
charToNote char
    | char == 'A' = A
    | char == 'B' = B
    | char == 'C' = C
    | char == 'D' = D
    | char == 'E' = E
    | char == 'F' = F
    | char == 'G' = G
    | otherwise = error "Invalid Note"


-- Convert Char to Octave (Int)
charToOctave :: Char -> Int
charToOctave char
    | char == '1' = 1
    | char == '2' = 2
    | char == '3' = 3
    | otherwise = error "Invalid Octave"


-- Convert Pitch-String to Pitch
strToPitch :: String -> Pitch
strToPitch pitch_str = Pitch note octave
    where
        note = charToNote (head pitch_str)
        octave = charToOctave (head (tail pitch_str))


initialGuess :: ([String], GameState)
initialGuess =
    ([p1_str, p2_str, p3_str], allChords)
    where
        poolSize = length allChords
        guess = allChords !! (quot poolSize 2) -- Mid point in the List as guess
        guess_l = Set.toList guess
        p1_str = show (guess_l !! 0)
        p2_str = show (guess_l !! 1)
        p3_str = show (guess_l !! 2)


nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess ([prevP1_str, prevP2_str, prevP3_str], prevChordPool) (cp, cn, co) =
    ([p1_str, p2_str, p3_str], chordPool)
    where
        prevP1 = strToPitch prevP1_str
        prevP2 = strToPitch prevP2_str
        prevP3 = strToPitch prevP3_str
        prevGuess = Set.fromList [prevP1, prevP2, prevP3]
        chordPool
            -- Filter out those Chords that have common pitches with previous guess if correct pitches = 0
            | cp == 0 = [chord | chord <- prevChordPool, Set.null (Set.intersection prevGuess chord)]
            -- Filter out those Chords that don't have common pitches with previous guess. Also exclude previous guess.
            | otherwise = [chord | chord <- prevChordPool, Set.null (Set.intersection prevGuess chord) == False && chord /= prevGuess]
        poolSize = length chordPool
        guess = chordPool !! (quot poolSize 2)
        guess_l = Set.toList guess
        p1_str = show (guess_l !! 0)
        p2_str = show (guess_l !! 1)
        p3_str = show (guess_l !! 2)
