module Utils where


-- Map with different functions for first, middle and last elements.
fmlMap :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
fmlMap f m l [] = []
fmlMap f m l [a] = [f a]
fmlMap f m l [a, b] = [f a, l b]
fmlMap f m l xs = [f (head xs)] ++ (m <$> (init (tail xs))) ++ [l (last xs)]
