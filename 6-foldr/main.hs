-- this code is not working.
-- main = print $ foldl (:) [] [1, 2, 3]

-- good
main = print $ foldr (:) [] [1, 2, 3]
