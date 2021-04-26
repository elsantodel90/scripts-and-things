import Data.List
import Data.Maybe

{-
type Player  = String
type Match   = (Player, Player)
type Date    = [Match]
type Fixture = [Date]

rotations l = take n . map (take n) . iterate tail $ cycle l
                    where n = length l

fixture players | n == 1         =  []
                | otherwise      = zipWith (++) (fixture p1) (fixture p2) ++ (map (zip p1) $ rotations p2)
                            where n  = length players
                                  n2 = n `div` 2
                                  (p1,p2) = splitAt n2 players


main = mapM_ print $ fixture ["juan","maria","jose","pepe"]

-}


type Player  = String
type Match   = (Player, Player)
type Date    = [Match]
type EvenFixture = [Date]
type OddFixture = ([Date],[Player])

noPlayer = "NO_PLAYER"

rotations l = take n . map (take n) . iterate tail $ cycle l
                    where n = length l

niceError msg n = error $ show n ++ msg
negativeError = niceError " should not be negative!"

hasNoPlayer (a,b) = noPlayer `elem` [a,b]
freePlayer (a,b) | a == noPlayer = b
                 | b == noPlayer = a
                 | otherwise     = error $ show (a,b) ++ "freePlayer called but both players exist"

-- Precondition: odd n
oddFixture :: [Player] -> OddFixture
oddFixture players | n < 0      = negativeError n
                   | even n     = niceError " should be odd!" n
                   | n == 1     =  ([[]],players)
                   | otherwise  =  (map (filter (not . hasNoPlayer)) ef,
                                    map (freePlayer . fromJust . find hasNoPlayer) ef)
                            where n       = length players
                                  ef      = evenFixture (noPlayer:players)

-- Precondition: even n
evenFixture :: [Player] -> EvenFixture
evenFixture players | n < 0     = negativeError n
                    | odd n     = niceError " should be even!" n
                    | n == 0    = []
                    | otherwise = if even n2
                                      then zipWith (++) (evenFixture p1) (evenFixture p2) ++ (map (zip p1) $ rotations p2)
                                      else let (fixture1,free1) = oddFixture p1
                                               (fixture2,free2) = oddFixture p2
                                               baseFixture      = zipWith (++) fixture1 fixture2
                                           in zipWith (:) (zip free1 free2) baseFixture ++ (map (zip free1) . tail $ rotations free2)
                                where n       = length players
                                      n2      = n `div` 2
                                      (p1,p2) = splitAt n2 players
                                      
                                      
main = print $ oddFixture ["juan","abel","cain"]
