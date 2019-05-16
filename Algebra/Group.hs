module Algebra.Group (
    Group(..),
    addToGroup,
    makeEmptyGroup,
    findGroupByCentroid,
    groupPoints,
    updateGroup
) where

import qualified Algebra.Point as AP
import Data.List
import Data.Function

data Group = Group {
    centroid :: AP.Point,
    points :: [AP.Point]
} deriving (Show, Eq, Ord)

addToGroup :: [AP.Point] -> Group -> Group
addToGroup pointsToAdd group = Group (centroid group) (points (group) ++ pointsToAdd)

makeEmptyGroup :: AP.Point -> Group
makeEmptyGroup point = Group point []

findGroupByCentroid :: [Group] -> AP.Point -> Group
findGroupByCentroid groups c = head [x | x <- groups, centroid x == c]

--updateGroup :: AP.Point -> [Group] -> AP.Point -> Group
updateGroup info = Group cent []
    where
        --group = findGroupByCentroid g c
        cent = snd $ head $ head $ info
        --poi = points group

--groupPoints :: [(AP.Point, AP.Point)] -> [Group] -> [Group]
groupPoints points groups = z
    where
        x = groupBy ((==) `on` snd) points
        z =  updateGroup x
        --z = map ([[(tupla)]])
        --a = map (\(ponto, centroide) -> updateGroup centroide groups ponto) points
        --c = map (findGroupByCentroid groups) [snd x | x <- points]
        --d = zip c ([map fst points])
        --e = map (\(grupo, pontos) -> addToGroup pontos grupo) d