import System.Random

data TaulerJoc = Taula [[Int]] deriving(Eq)

instance Show TaulerJoc where
    show t = imprim t
        where
        imprim::TaulerJoc -> String
        imprim (Taula []) = ""
        imprim (Taula (x:xs)) 
            | length xs > 1 = tractaFila x++"\n"++tractaFilaVert x++"\n"  ++ imprim (Taula (borrarFila xs)) --imprime una fila y envia a imprimir la siguiente
            | otherwise = tractaFila x++"\n" 
        
        tractaFila::[Int] ->String
        tractaFila ([]) = ""
        tractaFila (x:xs)
            | x <= 0 = ". "  ++ (tractaFila (borrarCol xs))
            | x == 2 = ". "  ++ (tractaFila (borrarCol xs))
            | otherwise = ".-" ++ (tractaFila (borrarCol xs))
            
        tractaFilaVert::[Int] ->String
        tractaFilaVert [] = ""
        tractaFilaVert (x:xs)
            | length xs > 0 && (mod x 100) >= 2 = "|" ++ show (head xs) ++ (tractaFilaVert (borrarCol xs))
            | length xs > 0  = " " ++ show (head xs) ++ (tractaFilaVert (borrarCol xs))
            | (mod x 100) >= 2 = "| " ++ (tractaFilaVert (borrarCol xs))
            | otherwise = "  " ++ (tractaFilaVert (borrarCol xs))
            
        borrarFila::[[Int]] -> [[Int]]
        borrarFila [] = []
        borrarFila (x:xs) = xs
        
        borrarCol::[Int] -> [Int]
        borrarCol [] = []
        borrarCol (x:xs) = xs
        
        tractaCuadrat::[Int] -> String
        tractaCuadrat ([]) = ""
        tractaCuadrat (x:xs) = show x ++ (tractaCuadrat (borrarCol xs))

buildTauler:: Int -> Int -> TaulerJoc -- 1 -> horiz der, 2 vert abaj (+100 p1 +200 si es p2)
buildTauler n m = (Taula (aux n m))
    where 
       aux::Int->Int->[[Int]]
       aux n m = replicate (n+n-1) (take (m+m-1) $ repeat 0)
{-     aux n m = (concat $ replicate m ([take (n+m) $ cycle [0,1]] ++ [take (n+m) $ cycle [2,0]]))
        ++ [take (n+m) $ cycle [0,1]]-}

        
        
placeLine::Int -> Int -> Int -> TaulerJoc -> Int-> TaulerJoc -- fila, columna, tipus linea, tauler, player
placeLine i j tipo tauler numPl
    | (posValid i j tauler) && (v /= tipo) && (v /= 3) && (tipo > 0 ) && (tipo < 3) = setValor i j tipo tauler numPl
    | otherwise = tauler
        where
        v = valorMatriz i j tauler

valorMatriz  :: Int -> Int -> TaulerJoc -> Int 
valorMatriz i j tauler@(Taula mat)
     | posValid i j tauler = (mat!!(i+i))!!(j+j)
     | otherwise = -1     
     
setValor:: Int -> Int -> Int -> TaulerJoc-> Int -> TaulerJoc
setValor i j tipo tauler@(Taula mat) numPl
    | posValid i j tauler = (Taula (updateMatrix mat tipo (i+i,j+j)))
    | otherwise = tauler
        where
            updateMatrix :: [[Int]] -> Int -> (Int, Int) -> [[Int]]
            updateMatrix m x (r,c) =
              take r m ++
              [take c (m !! r) ++ [(x + head (drop c (m !! r)))] ++ drop (c + 1) (m !! r)] ++
              drop (r + 1) m
            
        
posValid::Int -> Int -> TaulerJoc -> Bool
posValid i j (Taula mat) = i>=0 && ((length mat) > i+i) && j>=0 && (length (mat!!i) > j+j)
        
-----------------------------------------------------------------------------     

getPos:: Int -> Int -> TaulerJoc -> Int
getPos i j t@(Taula mat)
     | posValid i j t = (mat!!i)!!j
     | otherwise = -1


{-a partir de un tauler, retorna el numero del jugador que ha guanyat, o -1 si encara
no hi ha guanyador-}
isFinished::TaulerJoc ->Int
isFinished t@(Taula mat)
    | whoWon t 1 = 1
    | whoWon t 2 = 2
    | otherwise = -1
    where
        whoWon::TaulerJoc->Int -> Bool
        whoWon t color
            |color ==1 = checkWin (takeAdjacents 0 (-1) t 1 []) t color
            |otherwise = checkWin (takeAdjacents (-1) 0 t 2 []) t color
        checkWin:: [(Int,Int)] -> TaulerJoc ->Int ->Bool
        checkWin [] t color = False
        checkWin (x:xs) t color
            | color == 1 && isWinnerB (fst x) (snd x) t = True
            | color == 2 && isWinnerR (fst x) (snd x) t = True
            | otherwise = checkWin xs t color
        isWinnerB:: Int->Int ->TaulerJoc ->Bool
        isWinnerB i j t@(Taula mat)
            | i == (length mat) - 1 = True
            | otherwise = False
        isWinnerR::Int->Int->TaulerJoc->Bool
        isWinnerR i j t@(Taula mat)
            | j == (length (mat!!0))-1 = True
            | otherwise = False

takeAdjacents::Int -> Int->TaulerJoc ->Int-> [(Int,Int)]->[(Int,Int)]
takeAdjacents (-1) 0 t@(Taula mat) color [] = makeTheCall t color [] (take (div ((length mat)) 2) $ iterate ((+)2) 1)
    where
        makeTheCall::TaulerJoc -> Int -> [(Int,Int)] -> [Int] -> [(Int,Int)]
        makeTheCall t color visited (x:[]) = takeAdjacents x 0 t color visited
        makeTheCall t color visited (x:xs) = takeAdjacents x 0 t color visited ++ makeTheCall t color visited xs
takeAdjacents 0 (-1) t@(Taula mat) color [] = makeTheCall t color [] magic
    where
        magic = (take (div ((length (mat!!0))) 2) $ iterate ((+)2)1)
        makeTheCall::TaulerJoc -> Int -> [(Int,Int)] -> [Int] -> [(Int,Int)]
        makeTheCall t color visited (x:[]) = takeAdjacents 0 x t color visited
        makeTheCall t color visited (x:xs) = takeAdjacents 0 x t color visited ++ makeTheCall t color visited xs
takeAdjacents i j t@(Taula mat) color visited
    | not (elem upper visited) && getPos (i-1) j t == color = [upper]++takeAdjacents i j t color (visited++
                                                            [upper])++takeAdjacents (i-1) j t color (visited++[upper])
    | not (elem lower visited) && getPos (i+1) j t == color = [lower]++takeAdjacents i j t color (visited++[lower])++
                                                            takeAdjacents (i+1) j t color (visited++[lower])
    | not (elem left visited) && getPos i (j-1) t == color =[left]++takeAdjacents i j t color (visited++
                                                            [left])++takeAdjacents i (j-1) t color (visited++[left])
    | not (elem right visited) && getPos i (j+1) t == color = [right]++takeAdjacents i j t color (visited++[right])++
                                                            takeAdjacents i (j+1) t color (visited++[right])
    | otherwise =[]
    where
        upper = ((i-1),j)
        lower =((i+1),j)
        left = (i,(j-1))
        right = (i,(j+1))


--tots els gameloops tenen com a parametre el tauler a considerar y el torn del jugador. gameloops = estrategies

---------------------------------------

--retorna els posibles moviments del tauler actual
possiblesTiradas::TaulerJoc -> [((Int, Int),Int)]
possiblesTiradas tauler@(Taula mat) = buildList 0 0
    where 
        buildList::Int -> Int -> [((Int, Int),Int)]
        buildList i j  
            | (j+j == lenC-1) && (i + i == lenR-1) = [] -- ultima posicion de la matriz
            | (j+j == lenC-1) && ((valorMatriz i j tauler) /= 2) = [((i, j),2)] ++ (buildList i (j+1)) -- ultima columna
            | (j+j < lenC - 1) && (i + i == lenR-1) && ((valorMatriz i j tauler) /= 1) = [((i, j),1)] ++ (buildList i (j+1)) --ultima fila
            | (j+j < lenC - 1) && (i + i < lenR-1) && ((valorMatriz i j tauler) /= 3) = [((i, j),3 - (valorMatriz i j tauler))] ++ (buildList i (j+1)) --no ultima fila
            | (i + i + 2 == lenR-1) && ((valorMatriz (i+1) 0 tauler) /= 1) = [(((i+1), 0),1)] ++ (buildList (i+1) 1) -- ultima fila
            | (i + i + 2 < lenR - 1) && ((valorMatriz (i+1) 0 tauler) /= 3) = [(((i+1), 0),3 - (valorMatriz (i+1) 0 tauler))] ++ (buildList (i+1) 1) -- no ultima fila
            | (j+j < lenC-1) = buildList i (j+1)
            | otherwise = buildList (i+1) 0
        
        lenC = length (mat!!0)
        lenR = length mat

gameLoop1 tauler turn
    | isFinished tauler /= -1 =do
                          putStrLn ("Winner : Player "++show (isFinished tauler))
                          putStrLn $ show tauler
    | possiblesTiradas tauler ==[] = do --comprova si hi ha tirades possibles
                        putStrLn ("Empat")
                        putStrLn $ show tauler
    | otherwise = do --estrategia aleatoria
                putStrLn ("Player "++show turn++" moves")
                std<-newStdGen
                let newPos = (possiblesTiradas tauler)!!(fst (genera std 0 ((length (possiblesTiradas tauler))-1))) -- escoge una jugada de las posibles jugadas a partir del rand
                let valorTipo = fst (genera2 std (snd newPos))
                let newPos2 = (fst newPos,valorTipo)
                let newT = placeLine (fst (fst newPos)) (snd (fst newPos)) valorTipo tauler turn
                
                
                putStrLn ("Moved: "++show newPos2)
                putStrLn $show newT
                gameLoop1 newT (1+mod turn 2)
                
gameLoop2 tauler turn
    | isFinished tauler /= -1 =do
                          putStrLn ("Winner : Player "++show (isFinished tauler))
                          putStrLn $ show tauler
    | possiblesTiradas tauler ==[] = do
                        putStrLn ("Empat")
                        putStrLn $ show tauler
    | turn==1 = do
                putStrLn ("Player "++show turn++" moves")
                putStrLn "Board:"
                putStrLn $ show tauler
                putStrLn "select new position"
                x<-getLine
                y<-getLine
                putStrLn "select type (1 = horizontal to right, 2 = vertical to down)"
                z<-getLine
                let newT = (placeLine (read x ::Int) (read y::Int) (read z::Int) tauler turn)
                if newT == tauler
                    then gameLoop2 tauler turn
                    else gameLoop2 newT 2
    | otherwise = do
                  putStrLn ("Player "++show turn++" moves")
                  std<-newStdGen
                  let newPos = (possiblesTiradas tauler)!!(fst (genera std 0 ((length (possiblesTiradas tauler))-1))) -- escoge una jugada de las posibles jugadas a partir del rand
                  let valorTipo = fst (genera2 std (snd newPos))
                  let newPos2 = (fst newPos,valorTipo)
                  let newT = placeLine (fst (fst newPos)) (snd (fst newPos)) valorTipo tauler turn  
                  putStrLn ("Moved: "++show newPos2)
                  putStrLn $show newT
                  gameLoop2 newT 1


mode1 = do
    putStrLn "Enter dimensions: "
    files<-getLine
    columns<-getLine
    let tauler = buildTauler (read files :: Int) (read columns ::Int)
    gameLoop1 tauler 1

mode2 = do
    putStrLn "Enter dimensions: "
    files<-getLine
    columns<-getLine
    let tauler = buildTauler (read files :: Int) (read columns ::Int)
    gameLoop2 tauler 1

--generacio d'un nombre aleatori donat una seed 
genera :: RandomGen s => s -> Int -> Int -> (Int,s)
genera s lo hi = (x,s1)
    where
        (x,s1) = randomR (lo,hi) s
        
genera2 :: RandomGen s => s -> Int -> (Int,s)
genera2 s hi 
    | hi /= 3 = (hi,s1)
    | otherwise = (x,s1)
        where
            (x,s1) = randomR (1,2) s

modeSelector mode
    | mode ==1 = do mode1
    | otherwise = do mode2

main = do
    putStrLn "Select Mode"
    putStrLn "Mode 1: Random CPU vs Random CPU"
    putStrLn "Mode 2: Player vs Random CPU"
    mode<-getLine
    modeSelector (read mode::Int)
            
    
    
    
    
    
    
    
    
    
    
    
