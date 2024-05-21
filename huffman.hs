import Data.Word
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I

type Reg = (Word8, Word32)

data Huffman = Folha Int Char | No Int Huffman Huffman deriving Show

freqSimb [] = []
freqSimb (x:s) = [(Folha (length(filter (x==) (x:s))) x)]++(freqSimb (filter (x/=) (x:s)))

filterHuff _ [] = []
filterHuff f ((No i h1 h2):cauda) = if f i then [(No i h1 h2)]++filterHuff f cauda else filterHuff f cauda
filterHuff f ((Folha i c):cauda)  = if f i then [(Folha i c)]++filterHuff f cauda else filterHuff f cauda

quicksortHuff [] = []
quicksortHuff ((No i h1 h2):cauda) = (quicksortHuff menor) ++ [No i h1 h2] ++ (quicksortHuff maior)
    where
        menor  = filterHuff (< i) cauda
        maior = filterHuff (>= i) cauda
quicksortHuff ((Folha i c):cauda) = (quicksortHuff menor) ++ [Folha i c] ++ (quicksortHuff maior)
    where
        menor  = filterHuff (< i) cauda
        maior = filterHuff (>= i) cauda

construirArvore [(No i h1 h2)] = (No i h1 h2)
construirArvore ((No i1 h1 h2):(No i2 h3 h4):cauda) = construirArvore (quicksortHuff ([No (i1+i2) (No i1 h1 h2) (No i2 h3 h4)] ++ cauda))
construirArvore ((Folha i1 c1):(No i2 h1 h2):cauda) = construirArvore (quicksortHuff ([No (i1+i2) (Folha i1 c1) (No i2 h1 h2)] ++ cauda))
construirArvore ((No i1 h1 h2):(Folha i2 c2):cauda) = construirArvore (quicksortHuff ([No (i1+i2) (No i1 h1 h2) (Folha i2 c2)] ++ cauda))
construirArvore ((Folha i1 c1):(Folha i2 c2):cauda) = construirArvore (quicksortHuff ([No (i1+i2) (Folha i1 c1) (Folha i2 c2)] ++ cauda))

codHuffman (No i h1 h2) = (codHuffman' h1 "1") ++ (codHuffman' h2 "0")
   where
      codHuffman' (Folha i c) x = [(c,x)]
      codHuffman' (No i h1 h2) x = (codHuffman' h1 (x++"1")) ++ (codHuffman' h2 (x++"0"))

codificar s = codificaMesmo s (codHuffman (construirArvore (quicksortHuff (freqSimb s))))
   where
      codificaMesmo' x ((c,s):cauda) = if x == c then s else codificaMesmo' x cauda
      codificaMesmo [] _ = ""
      codificaMesmo (x:xs) ((c,s):cauda) = if x == c then s ++ (codificaMesmo xs ([(c,s)]++cauda)) else (codificaMesmo' x cauda) ++ (codificaMesmo xs ([(c,s)]++cauda))

decodificar [] _ _ = ""
decodificar (x:xs) (No i h1 h2) num = if x == '1' then decodificar' xs h1 (No i h1 h2) num else decodificar' xs h2 (No i h1 h2) num
   where
      decodificar' [] (Folha i c) (No j h1 h2) num = [c]
      decodificar' (x:xs) (No i h1 h2) (No j h3 h4) num = if x == '1' then decodificar' xs h1 (No j h3 h4) num else decodificar' xs h2 (No j h3 h4) num
      decodificar' xs (Folha i c) (No j h1 h2) num = if num /= 0 then [c] ++ (decodificar xs (No j h1 h2) (num-1))  else [c]

--- Starts Compressing File

compactaArquivo s = do 
      str <- readFile s
      --let vs = P.runPut (P.putWord8 (toEnum (descobreN (freq str)))
      --let vs = P.runPut (P.putWord32be (toEnum (descobreT lst)))
      let vs = P.runPut(putNT str)
      L.writeFile "teste.txt" vs

putNT str = do
      P.putWord8 (toEnum (descobreN (freq str)))
      P.putWord32be (toEnum (descobreT (freq str)))
      putF (freq str)
      putH [] (codificar str) 0
      
putF [] = P.flush
putF ((c,i):cauda) = do
      P.putWord8  (I.c2w c)
      P.putWord32be (toEnum i)
      putF cauda

putH [] [] _ = P.flush
putH z [] _ = do
      P.putWord8 (toEnum (strToIntH (z) 7))
      putH [] [] 0
putH z (a:cauda) i = do
      if i == 7 then putHZ (z++[a]) cauda else putH (z++[a]) cauda (i+1)

putHZ z cauda = do
      P.putWord8 (toEnum (strToIntH (z) 7))
      putH [] cauda 0

strToIntH _ (-1) = 0
strToIntH [] _ = 0
strToIntH (x:cauda) i = if x == '1' then (potSI i) + (strToIntH cauda (i-1)) else 0 + (strToIntH cauda (i-1))
   where 
      potSI 0 = 1
      potSI i = 2 * (potSI (i-1))

descobreN [] = 0
descobreN ((c,i):cauda) = 1 + (descobreN cauda)

descobreT [] = 0
descobreT ((c,i):cauda) = i + (descobreT cauda)

--- Finishes compressing File

-- Starts reading File

lerArquivoCompactado s = do
      bs <- L.readFile s
      let rs = G.runGet getRegsH bs
      return rs

getRegH = do 
      c <- G.getWord8
      f <- G.getWord32be
      return (c,f)

getRegN = do 
      n <- G.getWord8
      return n

getRegT = do 
      t <- G.getWord32be
      return t

getArv n = do
      if n == 1 then do {c <- G.getWord8; i <- G.getWord32be; return [(Folha (fromIntegral i) (I.w2c c))] }
                else do {c <- G.getWord8; i <- G.getWord32be; arvr <- (getArv (n-1)); return ([(Folha (fromIntegral i) (I.w2c c))]++arvr)}

getCod = do
      empty <- G.isEmpty
      if empty then return []
               else do{t <- G.getWord8; ts <- getCod; return ([fromEnum t]++ts)}

getRegsH = do
  empty <- G.isEmpty
  if empty then return []
           else do {n <- getRegN; t <- getRegT; arv <- (getArv (fromIntegral n)); cod <- getCod; rt <- (decodificar (transfH cod) (construirArvore arv) (fromIntegral t)); return rt}

--[(cod,arv,t)]


transfH (x:xs) = (toBin x)++(transfH xs)
toBin 0 = "0"
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ "1"
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ "0"

-- End of reading file

escrita = do
   txt <- readFile "Arv.hs"
   let xs = freq txt   
   let bs = P.runPut (put (freq txt))
   L.writeFile "teste.txt" bs 

freq [] = []   
freq (x:xs) = (x, length (filter (==x) xs) + 1): freq (filter (/=x) xs)

put [] = P.flush
put ((c, f):xs) = do
  P.putWord8  (I.c2w c)
  P.putWord32be (toEnum f) -- Big Endian
  put xs

leitura = do 
  bs <- L.readFile "teste.txt"
  let rs = G.runGet getRegs bs
  printRegs rs  
  
printRegs [] = return ()
printRegs (r:rs) = do 
   printReg r
   printRegs rs                      

printReg (c, f) = putStrLn ((show (I.w2c c)) ++ " - " ++ show f)
  
  
--getReg :: G.Get Reg
getReg = do 
  c <- G.getWord8
  f <- G.getWord32be
  return (c,f)

--getRegs :: G.Get [Reg]
getRegs = do 
  empty <- G.isEmpty
  if empty then return []
           else do {r <- getReg; rs <- getRegs; return (r:rs)}

--decodificar (codificar "sststringg") (construirArvore(quicksortHuff(freqSimb "sststringg")))
--decodificar "0000110011010101000111010" (No 10 (No 4 (Folha 2 't') (Folha 2 'g')) (No 6 (No 3 (Folha 1 'n') (No 2 (Folha 1 'r') (Folha 1 'i'))) (Folha 3 's')))

