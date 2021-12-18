--  File    : Proj2.hs
--  Author  : Oliver Bestel de Lezongard (914956,obestel@student.unimelb.edu.au)
--  Purpose : COMP30020 project to solve a simple guessing game
--  Grade : 97/100

--  The following program implements code to solve a simple logical guessing 
--  game akin to Battleship on a 4x8 grid. Three ships are on the grid. 
--  A guess/target consists of three locations on a [A..H] X [1..4] grid.
--  For example, the guess [A1,A4,H4] includes the top left, bottom 
--  left, and bottom right corners respectively. After every guess feedback is 
--  recieved on the number of ships exactly located, the number of ships exactly
--  one space away, the number of ships exactly two spaces away. Each guess is 
--  only counted as its closest distance to any ship. Diagonally adjacent moves
--  are counted as a single move. 

--  The role of the program is to guess the ships location with repeated guesses
--  that are based on the feedback of the previous guess. This is achieved by 
--  picking guesses/targets that on average leave you with the smallest set of 
--  remaining possible locations for the ships. 

module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess, getBestGuess) where


import Data.Char
import Data.List
import Data.Maybe


--  Location represents a square on the grid. Each square is denoted by a  
--  string made out of a column component ([A..H]) and a row componet ([1..4]).
type Location = String  


--  GameState is a list of a list of locations and is used throughout the
--  program to store sets of possible guesses. 
type GameState =  [[Location]]


--  Converts an input String to Just Location if it is in the set of valid
--  possible locations, or Nothing if it an invalid location. 
toLocation:: String -> Maybe Location
toLocation x 
    | elem x validLocations = Just x
    | otherwise = Nothing 
     where 
     validLocations = ["A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", 
       "C1", "C2", "C3", "C4", "D1", "D2", "D3", "D4", "E1", "E2", "E3", "E4",
       "F1", "F2" , "F3", "F4", "G1", "G2", "G3", "G4", "H1", "H2", "H3", "H4"]

--  Converts a Location to a two-character String respresentation of the 
--  Location.
fromLocation:: Location -> String 
fromLocation (x) = x


--  Takes a guess and target conisting of three locations each and returns the 
--  number of ships exactly located, the number of ships exactly one space away,
--  the number of ships exactly two spaces away. Implements a helper function 
--  to determine the distance between two Locations.
feedback:: [Location] -> [Location] -> (Int, Int, Int) 
feedback target guess  =
    (zero, one, two)
      where 
      g1 = guess \\  [x| x <- guess, y <- target, locationDistance x y == 0]
      g2 = g1 \\ [x| x <- guess, y <- target, locationDistance x y == 1]
      zero = length[x| x <- guess, y <- target, locationDistance x y == 0]
      one = length(nub [x| x <- g1 , y <- target, locationDistance x y == 1])
      two = length(nub [x| x <- g2, y <- target, locationDistance x y == 2])


--  Generates the initial guess used for the guessing game, and generates the
--  initial GameState that contains all 4960 (32 choose 3) possible guesses.
--  The initial guess is a tested initial guess that is chosen so as to leave 
--  on average the smallest remaining set of set of possible targets.   
initialGuess:: ([Location], GameState)
initialGuess = 
    (["A1","A3", "H1"], allTargets)
     where 
     locations = ["A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", 
       "C1", "C2", "C3", "C4", "D1", "D2", "D3", "D4", "E1", "E2", "E3", "E4",
       "F1", "F2" , "F3", "F4", "G1", "G2", "G3", "G4", "H1", "H2", "H3", "H4"]
     allTargets =  (nub ([ sort [x, y, z] |
       x <- locations, y <- locations, z <- locations, x /= y, x /= z, y /= z]))  
       


--  Generates a guess based on feedback and gamestate of the previous guess.
--  It takes advantage of symmetry within the problem space by only 
--  including targets if they share the same feedback as the previous guess.
--  It then uses a helper function to select a guess that leaves the smallest 
--  remaining set of possible targets. 
nextGuess:: ([Location], GameState) -> (Int,Int,Int) -> ([Location], GameState)
nextGuess (guess, gamestate) (a,b,c) = 
    (newGuess, newGState)
    where 
     newGState = delete guess ([x| x <- gamestate, feedback x guess == (a,b,c)])
     newGuess = guessFinder newGState
    
       








--  Calculates the distance between two letters that are used to denote the
--  column position. 
letterDistance:: Char -> Char -> Int 
letterDistance a b =
    abs((fromMaybe (99) (elemIndex a letters)) 
    - (fromMaybe (99) (elemIndex b letters)))
     where letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']


--  Calculates the correct distance between two locations if they are within two
--  spaces. Only spaces that are zero, one, or two spaces away from
--  the eachother are important. Diagonally adjacent blocks have a distance 
--  between them equal to one. 
locationDistance:: Location -> Location -> Int 
locationDistance (a) (b) 
    | (letterDistance (head a) (head b)) == 1 
     && abs( digitToInt(last a)-digitToInt(last b)) == 1 = 1
    | (letterDistance (head a) (head b)) == 1 
     && abs( digitToInt(last a)-digitToInt(last b)) == 2 = 2
    | (letterDistance (head a) (head b)) == 2 
     && abs( digitToInt(last a)-digitToInt(last b)) == 2 = 2 
    | (letterDistance (head a) (head b)) == 2 
     && abs( digitToInt(last a)-digitToInt(last b)) == 1 = 2 
    | otherwise = (letterDistance (head a) (head b)) 
     + abs( digitToInt(last a)-digitToInt(last b))





--  Generates a guess that on average leaves the smallest set of possible 
--  targets by calculating the average number of remaining targets for each 
--  guess and selecting the option with the smallest set of remaining targets 
--  on average. 
guessFinder:: GameState -> [Location]
guessFinder gState = 
    guess
     where 
     targets = [( (avgPosTarg target (gState \\ [target])), target ) | 
      target <- gState ]
     guess = snd ( head (sort targets))
     
               
--  Generates the average number of possible targets that will remain after a 
--  guess in a given GameState.
avgPosTarg:: [Location] -> GameState -> Float 
avgPosTarg target gState =
    sum [(fromIntegral (length x)/allFbacks)* fromIntegral (length x) | 
     x <- gFbacks ]    
     where
      feedbacks  = [feedback x target | x <- gState]
      gFbacks =  group (sort feedbacks)
      allFbacks = fromIntegral (length feedbacks)






--  Function used to calculate the best initial guess by calculating the average 
--  number in the set of remaining possible targets after the guess. The 
--  variables are used to iterate through all 4960 choices in sets of 100 so as 
--  to prevent halted execution due to SIGKILL.
getBestGuess ::Int -> Int -> (Float,[Location]) 
getBestGuess tk drp =   
     head (set)
     where 
      locations = ["A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", 
       "C1", "C2", "C3", "C4", "D1", "D2", "D3", "D4", "E1", "E2", "E3", "E4",
       "F1", "F2" , "F3", "F4", "G1", "G2", "G3", "G4", "H1", "H2", "H3", "H4"]
      xs = (nub ([ sort [x, y, z] |x <- locations, y <- locations,  
       z <- locations, x /= y, x /= z, y /= z]))  
      set =  (sort[(c, target) | target <- drop drp (take tk xs),  
       let c = avgPosTarg target xs])
     
  
