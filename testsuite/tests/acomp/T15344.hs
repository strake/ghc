{-# LANGUAGE ApplicativeComprehensions #-}

f :: Maybe (Maybe Int) -> Maybe Int -> Maybe Int
f mgs mid = [ moi + 42 | _ <- mid, Just moi <- mgs ]

main :: IO ()
main = print (f (Just Nothing) (Just 2))
