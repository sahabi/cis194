module Buffer where

class Buffer b where
    toString :: b -> String

    fromString :: String -> b

    line :: Int -> b -> Maybe String

    replaceLine :: Int -> String -> b -> b

    numLines :: b -> Int

    value :: b -> Int
