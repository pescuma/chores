module Chores where

import qualified Data.Map as Map
import           NextGen

data Chore = Chore {
    getTitle       :: String,
    getDescription :: String,
    getAttributes  :: Map.Map String String
}

instance Show Chore where
    show (Chore t d a) = "Chore { title=" ++ t
                         ++ (a
                             & Map.insert "description" d
                             & Map.toList
                             -- & filter (\(_, v) -> v /= "")
                             & concatMap (\(k, v) -> ", "++ k ++ "=" ++ v)
                         ) ++ " }"


createChore :: String -> Chore
createChore t = Chore t "" Map.empty

setTitle :: String -> Chore -> Chore
setTitle t (Chore _ d a) = Chore t d a

setDescription :: String -> Chore -> Chore
setDescription d (Chore t _ a) = Chore t d a

getAttribute :: String -> Chore -> Maybe String
getAttribute n (Chore _ _ a) = Map.lookup n a

getAttributeOrDefault :: String -> String -> Chore -> String
getAttributeOrDefault d n (Chore _ _ a) = Map.findWithDefault d n a

setAttribute :: String -> String -> Chore -> Chore
setAttribute n v (Chore t d a) = Chore t d (Map.insert n v a)

