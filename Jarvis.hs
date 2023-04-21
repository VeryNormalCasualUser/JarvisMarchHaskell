module Jarvis where
import CreateInputs
import Dims  

import Data.Maybe ( fromJust )


findXMost :: (Double -> Double -> Bool) -> [Dim] -> (Maybe Dim, [Dim]) --dropsFoundElement
findXMost _ [] = (Nothing, []) 
findXMost comparator (x:xs) = 
    findXMost' [] xs x
    where 
        findXMost' retList [] currentElem = (Just currentElem, retList)
        findXMost' retList (y:ys) currentElem
            |comparator (getX y) (getX currentElem)     = findXMost' (currentElem : retList) ys y
            |otherwise                                  = findXMost' (y:retList) ys currentElem 


rightMost   :: [Dim] -> (Maybe Dim, [Dim])
rightMost   = findXMost (>) 
leftMost    :: [Dim] -> (Maybe Dim, [Dim])
leftMost    = findXMost (<) 

checkNextPoint :: Dim -> [Dim] -> (Dim, [Dim])
checkNextPoint onHullP points = onHull onHullP (head points) [] (tail points)


onHull :: Dim -> Dim -> [Dim] ->  [Dim] -> (Dim, [Dim]) 

onHull onHullP currNext leftOverPoints [] = (currNext, leftOverPoints)

onHull onHullP currNext leftOverPoints (nextCand:points) 
    |p_onHull_to_currNext == p_onHull_to_nextCand 
        = onHull onHullP nextCand (leftOverPoints ++ [nextCand]) points
    |isScaled      p_onHull_to_currNext p_onHull_to_nextCand 
        = onHull onHullP (max currNext nextCand) (min currNext nextCand : leftOverPoints) points
    | isRightTurn  p_onHull_to_currNext p_onHull_to_nextCand   
        = onHull onHullP nextCand (leftOverPoints ++ [currNext]) points
    | otherwise                                               
        = onHull onHullP currNext (leftOverPoints ++ [nextCand]) points
    where 
        p_onHull_to_currNext = onHullP --> currNext
        p_onHull_to_nextCand = onHullP --> nextCand    



-- isScaled    p_onHull_to_currNext p_onHull_to_nextCand = onHull onHullP (max currNext nextCand) (min currNext nextCand : leftOverPoints) points



jarvisWalk :: [Dim] -> [Dim]
jarvisWalk [] = []
jarvisWalk [x] = [x]
jarvisWalk [x,y] = [x,y]
jarvisWalk [x,y,z] = [x,y,z]
jarvisWalk pointList =
    jarvisWalkHelper [startingPoint] startingPoint startingPoint (startList++[startingPoint]) 
    where
        leftMostRes     = leftMost pointList
        startingPoint   = fromJust $ fst leftMostRes
        startList       = snd leftMostRes 
        jarvisWalkHelper result startPoint currPoint [] = result
        jarvisWalkHelper result startPoint currPoint remainingPointList 
            |nextPoint == startPoint = result
            |otherwise = jarvisWalkHelper (nextPoint : result) startPoint nextPoint restList
            where 
                nextPointAndList = checkNextPoint currPoint remainingPointList
                nextPoint = fst nextPointAndList
                restList = snd nextPointAndList



