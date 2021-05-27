{- Importaciones de librerias necesarias para el funcionamiento
    Reference: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html
    Reference: https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html
  -}
module Kata where
import Data.List
import System.IO

{- Hacer los valores en celdas de 3x3 para cada uno de los valores
    en un tipo de arreglo, uno para cada digito.
  -}
value_one =["   ",
            "  |", 
            "  |"]  

value_two = [" _ ",
             " _|", 
             "|_ "]  

value_three = [" _ ",
               " _|", 
               " _|"] 

value_four = ["   ",
              "|_|", 
              "  |"] 

value_five = [" _ ",
              "|_ ", 
              " _|"]                          
        
value_six =[" _ ",
            "|_ ", 
            "|_|"]

value_seven = [" _ ",
               "  |", 
               "  |"]  

value_eight = [" _ ",
               "|_|", 
               "|_|"]

value_nine = [" _ ",
              "|_|", 
              "  |"]              

value_zero = [" _ ",
              "| |", 
              "|_|"]


{- Funcion desarollada para comparar el parseo del digito con cada parte de
    de los arreglos, asi saber cual identificar
  -}
valueFunc digit = if digit == value_one 
                then 1
        else if digit == value_two
                then 2
        else if digit == value_three
                then 3
        else if digit == value_four
                then 4
        else if digit == value_five
                then 5
        else if digit == value_six
                then 6
        else if digit == value_seven
                then 7
        else if digit == value_eight
                then 8
        else if digit == value_nine
                then 9
        else if digit == value_zero
                then 0
                else 0


{- Funcion para abrir el archivo que recibe como parametro de entrada
    y dibujar su contenido
    Reference: https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html
  -}
echoFile :: FilePath -> IO ()
echoFile ruta = do 
    contenido <- readFile ruta
    putStrLn "=> Cuenta en OCR: "
    putStrLn contenido  
    
{- Ir leyendo el parseo por cada renglon y respetando el salto
    de 27 cada linea + 1 de salto de linea para el siguiente
    renglon
  -}
    let renglon1 = take 27 contenido 
    let fin1 = drop 28 contenido
    let renglon2 = take 27 fin1
    let fin2 = drop 28 fin1
    let ultimoRenglon = take 27 fin2

{- Para ir leyendo de 3 en 3 los caracteres de cada renglon
    para ir formando el digito (renglon1)
  -}
    let (num1,move1) = splitAt 3 renglon1
    let (num2,move2) = splitAt 3 move1
    let (num3,move3) = splitAt 3 move2
    let (num4,move4) = splitAt 3 move3
    let (num5,move5) = splitAt 3 move4
    let (num6,move6) = splitAt 3 move5
    let (num7,move7) = splitAt 3 move6
    let (num8,move8) = splitAt 3 move7
    let num9 = move8

{- Para ir leyendo de 3 en 3 los caracteres de cada renglon
    para ir formando el digito (renglon2)
  -}
    let (num_1,movee1) = splitAt 3 renglon2
    let (num_2,movee2) = splitAt 3 movee1
    let (num_3,movee3) = splitAt 3 movee2
    let (num_4,movee4) = splitAt 3 movee3
    let (num_5,movee5) = splitAt 3 movee4
    let (num_6,movee6) = splitAt 3 movee5
    let (num_7,movee7) = splitAt 3 movee6
    let (num_8,movee8) = splitAt 3 movee7
    let num_9 = movee8

{- Para ir leyendo de 3 en 3 los caracteres de cada renglon
    para ir formando el digito (ultimorenglon)
  -}
    let (num__1,moveee1) = splitAt 3 ultimoRenglon
    let (num__2,moveee2) = splitAt 3 moveee1
    let (num__3,moveee3) = splitAt 3 moveee2
    let (num__4,moveee4) = splitAt 3 moveee3
    let (num__5,moveee5) = splitAt 3 moveee4
    let (num__6,moveee6) = splitAt 3 moveee5
    let (num__7,moveee7) = splitAt 3 moveee6
    let (num__8,moveee8) = splitAt 3 moveee7
    let num__9 = moveee8

{- Formar la figura obtenida
  -}
    let value1 = [num1,num_1,num__1]
    let value2 = [num2,num_2,num__2]
    let value3 = [num3,num_3,num__3]
    let value4 = [num4,num_4,num__4]
    let value5 = [num5,num_5,num__5]
    let value6 = [num6,num_6,num__6]
    let value7 = [num7,num_7,num__7]
    let value8 = [num8,num_8,num__8]
    let value9 = [num9,num_9,num__9]

    {- Imprimir el valor en decimal que esta contenido en el arreglo
    que se formo
  -}
    
    putStrLn "\n=> Numero de cuenta decimal:"
    let decimal = [valueFunc value1,valueFunc value2,valueFunc value3,valueFunc value4,valueFunc value5,valueFunc value6,valueFunc value7,valueFunc value8,valueFunc value9]
    let pos0=(decimal!!0)
    let pos1=(decimal!!1)
    let pos2=(decimal!!2)
    let pos3=(decimal!!3)
    let pos4=(decimal!!4)
    let pos5=(decimal!!5)
    let pos6=(decimal!!6)
    let pos7=(decimal!!7)
    let pos8=(decimal!!8)
    print((show pos0) ++ (show pos1)++ (show pos2)++ (show pos3)++ (show pos4)++ (show pos5)++ (show pos6)++ (show pos7)++ (show pos8))
    putStrLn "\n"
    -- Fin ejecucion