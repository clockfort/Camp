
{-
XXX This program is just a quick hack. It can probably be written much
more efficiently, and much more nicely
-}

module Camp.View.Draw.Text (draw) where

import Camp.Patch.Name
import Camp.Patch.Pretty

import Camp.View.Types

draw :: Mapping Name DepInfo -> IO ()
draw m = do let maxNameLength = maximum $ map (length . pprint . fst) m
            draw' (maxNameLength + 2) [] m

colWidth :: Int
colWidth = 4

draw' :: Int -> [(Name, Int)] -> Mapping Name DepInfo -> IO ()
draw' _ [] [] = return ()
draw' _ _  [] = error "Can't happen: draw': Dangling lines"
draw' indent cols0 ((patchName, (noRevDeps, deps)) : ms)
    = do let cols1 = dropDeadCols cols0 deps
             cols2 = addColumn patchName cols1
             cols3 = if noRevDeps then cols1 else cols2
             patchCol = case lookup patchName cols2 of
                        Just col -> col
                        Nothing -> error "Can't happen: draw': No patchName"
             depCols = depColumns deps cols0
             horizBarStart = minimum (patchCol : depCols)
             horizBarStop  = maximum (patchCol : depCols)
         printDeps indent cols0 horizBarStart horizBarStop patchCol deps
         printPatch indent cols2 patchName
         draw' indent cols3 ms

-- XXX Rewrite with filter
dropDeadCols :: [(Name, Int)] -> [(Name, Bool)] -> [(Name, Int)]
dropDeadCols [] _ = []
dropDeadCols (col@(n, _) : cols) deps = case lookup n deps of
                                        Just True -> cols'
                                        _ -> col : cols'
    where cols' = dropDeadCols cols deps

depColumns :: [(Name, Bool)] -> [(Name, Int)] -> [Int]
depColumns deps = f
    where depNames = map fst deps
          f [] = []
          f ((n, i) : xs)
              = if n `elem` depNames then i : f xs
                                     else     f xs

printDeps :: Int -> [(Name, Int)] -> Int -> Int -> Int -> [(Name, Bool)]
          -> IO ()
printDeps indent cols hStart hStop patchCol deps
 = do putStr (replicate indent ' ')
      f 0 cols
    where depNames = map fst deps
          f offset [] = if offset < colWidth * hStop
                        then let shift = colWidth * hStop - offset
                             in do putStr $ replicate shift '-'
                                   putChar '+'
                                   putStrLn ""
                        else putStrLn ""
          f offset ((n, i) : xs)
           | (offset < colWidth * hStart) && (hStart < i)
              = do let shift = colWidth * hStart - offset
                   putStr $ replicate shift ' '
                   f (colWidth * hStart) ((n, i) : xs)
           | (offset < colWidth * hStop) && (hStop < i)
              = do let shift = colWidth * hStop - offset
                   putStr $ replicate shift '-'
                   f (colWidth * hStop) ((n, i) : xs)
           | otherwise
              = do let shift = colWidth * i - offset
                       shiftChar = if (colWidth * hStart <= offset) &&
                                      (colWidth * hStop >= offset)
                                   then '-'
                                   else ' '
                   putStr $ replicate shift shiftChar
                   let myChar = case (n `elem` depNames, i == patchCol) of
                                (True,  True)  -> '|'
                                (True,  False) -> '*'
                                (False, True)  -> '+'
                                (False, False) -> '|'
                   putChar myChar
                   f (offset + shift + 1) xs

printPatch :: Int -> [(Name, Int)] -> Name -> IO ()
printPatch indent cols patchName = do putStr $ pad indent $ pprint patchName
                                      f 0 cols
    where f _ [] = putStrLn ""
          f offset ((n, i) : xs) = do let shift = colWidth * i - offset
                                      putStr $ replicate shift ' '
                                      if n == patchName
                                          then putChar 'O'
                                          else putChar '|'
                                      f (offset + shift + 1) xs

-- XXX Should be in a Utils module
pad :: Int -> String -> String
pad i str = str ++ replicate (i - length str) ' '

addColumn :: Name -> [(Name, Int)] -> [(Name, Int)]
addColumn patchName = f 0
    where f i [] = [(patchName, i)]
          f i xs@(x@(_, j) : xs')
           | i == j    = x : f (i + 1) xs'
           | otherwise = (patchName, i) : xs

