
{-
XXX This program is just a quick hack. It can probably be written much
more efficiently, and much more nicely
-}

module Camp.View.Draw.PIL (draw) where

import Camp.Patch.Name
import Camp.Patch.Pretty

import Camp.View.Types

import Control.Monad
import System.Cmd
import System.Exit
import System.IO

draw :: Mapping Name DepInfo -> IO ()
draw m = do h <- openFile "camp-view.py" WriteMode
            hPutStrLn h "#!/usr/bin/python"
            hPutStrLn h ""
            hPutStrLn h "import Image, ImageFont, ImageDraw"
            hPutStrLn h ""
            hPutStrLn h "image = Image.new('RGB', (200, 200), '#FFFFFF')"
            hPutStrLn h "draw = ImageDraw.Draw(image)"
            hPutStrLn h ""
            draw' h 0 [] m
            hPutStrLn h ""
            hPutStrLn h "image.save('camp-view.png', 'PNG')"
            hClose h
            rPython <- rawSystem "python" ["camp-view.py"]
            case rPython of
                ExitSuccess ->
                    do rQiv <- rawSystem "qiv" ["camp-view.png"]
                       case rQiv of
                           ExitSuccess ->
                               return ()
                           f ->
                               error ("Failed to run qiv: " ++ show f)
                f ->
                    error ("Failed to run python: " ++ show f)

draw' :: Handle -> Int -> [(Name, Int)] -> Mapping Name DepInfo -> IO ()
draw' _ _ [] [] = return ()
draw' _ _ _  [] = error "Can't happen: draw': Dangling lines"
draw' h row cols0 ((patchName, (noRevDeps, deps)) : ms)
    = do let cols1 = dropDeadCols cols0 deps
             cols2 = addColumn patchName cols1
             cols3 = if noRevDeps then cols1 else cols2
             patchCol = case lookup patchName cols2 of
                        Just col -> col
                        Nothing -> error "Can't happen: draw': No patchName"
             depCols = depColumns deps cols0
             horizBarStart = minimum (patchCol : depCols)
             horizBarStop  = maximum (patchCol : depCols)
         printDeps h row cols0 deps
         printHorizBar h row horizBarStart horizBarStop
         printPatch h row cols2 patchName
         draw' h (row + 1) cols3 ms

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

printDeps :: Handle -> Int -> [(Name, Int)] -> [(Name, Bool)] -> IO ()
printDeps h row cols deps
 = f cols
    where depNames = map fst deps
          f [] = return ()
          f ((n, i) : xs)
              = do let x = indent + cellWidth * i + (cellWidth `div` 2)
                       yTop = row * rowHeight
                       yMiddle = yTop + (interCellHeight `div` 2)
                       yBottom = case lookup n deps of
                                 Just True -> yTop + (interCellHeight `div` 2)
                                 _         -> yTop + interCellHeight
                       radius = 2
                       ellipseBB = [x - radius, yMiddle - radius,
                                    x + radius, yMiddle + radius]
                   hPutStrLn h ("draw.line([" ++
                                show (x, yTop) ++ ", " ++
                                show (x, yBottom) ++
                                "], fill='#000000', width=1)")
                   when (n `elem` depNames) $
                       hPutStrLn h ("draw.ellipse(" ++
                                    show ellipseBB ++
                                    ", fill='#000000')")
                   f xs

printHorizBar :: Handle -> Int -> Int -> Int -> IO ()
printHorizBar h row horizBarStart horizBarStop
 = do let xLeft  = indent + cellWidth * horizBarStart + (cellWidth `div` 2)
          xRight = indent + cellWidth * horizBarStop + (cellWidth `div` 2)
          y = row * rowHeight + (interCellHeight `div` 2)
      hPutStrLn h ("draw.line([" ++
                   show (xLeft, y) ++ ", " ++
                   show (xRight, y) ++
                   "], fill='#000000', width=1)")

printPatch :: Handle -> Int -> [(Name, Int)] -> Name -> IO ()
printPatch h row cols patchName
 = do let x = 5
      hPutStrLn h ("draw.text(" ++ show (x, yTop) ++ ", \"" ++
                   pprint patchName ++ "\", fill='#000000')")
      f cols
    where yVeryTop = yTop - (interCellHeight `div` 2)
          yTop     = row * rowHeight + interCellHeight
          yBottom  = yTop + cellHeight
          f [] = return ()
          f ((n, i) : xs)
              = do let xMiddle = indent + cellWidth * i + (cellWidth `div` 2)
                       -- height, not width, as we want a circle
                       radius  = cellHeight `div` 2
                       xLeft   = xMiddle - radius
                       xRight  = xMiddle + radius
                   if n == patchName
                       then do hPutStrLn h ("draw.line([" ++
                                           show (xMiddle, yVeryTop) ++ ", " ++
                                           show (xMiddle, yTop) ++
                                           "], fill='#000000', width=1)")
                               hPutStrLn h ("draw.ellipse(" ++
                                            show [xLeft, yTop,
                                                  xRight, yBottom] ++
                                            ", fill='#0000FF')")
                       else hPutStrLn h ("draw.line([" ++
                                         show (xMiddle, yTop) ++ ", " ++
                                         show (xMiddle, yBottom) ++
                                         "], fill='#000000', width=1)")
                   f xs

addColumn :: Name -> [(Name, Int)] -> [(Name, Int)]
addColumn patchName = f 0
    where f i [] = [(patchName, i)]
          f i xs@(x@(_, j) : xs')
           | i == j    = x : f (i + 1) xs'
           | otherwise = (patchName, i) : xs

rowHeight :: Int
rowHeight = cellHeight + interCellHeight

interCellHeight :: Int
interCellHeight = 15

cellHeight :: Int
cellHeight = 10

cellWidth :: Int
cellWidth = 30

indent :: Int
indent = 120

