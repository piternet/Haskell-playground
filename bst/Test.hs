import BST

l = [5, 3, 1, 5, 8, 2]

main = do
       putStrLn "testing sorting using BST..."
       putStr "before: l = " 
       print l
       putStr "after : l = "
       let sortedList = (sortBST l)
       print $ sortBST sortedList
       putStrLn $ if check sortedList then "OK, sorted" else "Error, not sorted"

check :: (Ord a, Num a) => [a] -> Bool 
check l = _check l (-1) True 
    where
        _check [] _ acc = acc
        _check (h:t) prev acc = 
            _check t h ((h >= prev) && acc)