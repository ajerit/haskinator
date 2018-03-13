-- Universidad Simón Bolívar
-- Laboratorio de Lenguajes de Programación I
-- Proyecto 1 - Haskinator
-- Autor: Adolfo Jeritson. 12-10523

module Main (main) where
  import Data.Char
  import Data.List
  import System.Exit
  import System.IO  
  import Oraculo
  import System.Directory
  import qualified Data.Map as Map

  main :: IO()
  main = do -- Mensaje de bienvenida
    putStrLn "\nEn las profundidades del bosque de los mil y un monads,"
    putStrLn "en la apartada y escondida cuna del gran río Curry,"
    putStrLn "vive en su choza de piedra el poderoso oráculo Haskinator."
    cicloHaskinator $ crearOraculo ""

  -- Función auxiliar del menú
  cicloHaskinator :: Oraculo -> IO ()
  cicloHaskinator oraculo = do
    putStrLn "\n¿Qué desea hacer con el poderoso Haskinator?\n"

    putStrLn "1.- Crear Oráculo"
    putStrLn "2.- Predecir"
    putStrLn "3.- Persistir"
    putStrLn "4.- Cargar"
    putStrLn "5.- Consultar Pregunta Crucial"
    putStrLn "6.- Salir"

    putStrLn "\nIngrese solo el número:"
    opcion <- getLine

    case opcion of
      "1" -> crearPrediccion
      "2" -> do
        nuevoOraculo <- (predecir oraculo)
        cicloHaskinator nuevoOraculo
      "3" -> persistir oraculo
      "4" -> do 
        nuevoOraculo <- cargar
        cicloHaskinator nuevoOraculo
      "5" -> consultar oraculo
      "6" -> exitSuccess
      ---------------------------------------
      -- debug: mostrar el oraculo actual
      ---------------------------------------
     {-  "d" -> do
        putStrLn ""
        putStrLn $ show oraculo
        cicloHaskinator oraculo -}
      ---------------------------------------
      _   -> do -- Respuesta no válida
        putStrLn "Opción invalida."
        cicloHaskinator oraculo


  -- Crea un Oráculo Predicción nuevo, con el texto de usuario
  crearPrediccion :: IO ()
  crearPrediccion = do
    putStrLn "\nIngrese el texto de la predicción:"
    pred <- getLine
    cicloHaskinator (crearOraculo pred)

  -- Recibe el Oráculo actual y dependiendo del tipo realiza 
  -- las acciones correspondientes. Si recibe uno vacío retorna al menú
  predecir :: Oraculo -> IO Oraculo
  predecir (Pregunta txt ops) = predecirPreg (Pregunta txt ops)
  predecir (Prediccion txt) = 
    if txt == "" then do -- Si es vacío regresa al menú
      putStrLn "\nError: El Oráculo está vacío.\nCree uno nuevo antes de predecir."
      return (Prediccion txt)
    else
      predecirPred (Prediccion txt)

  -- Predicción para Oráculo de tipo Pregunta.
  -- Retorna un Oráculo nuevo con los cambios correspondientes al 
  -- proceso de predicción
  predecirPreg :: Oraculo -> IO Oraculo
  predecirPreg oraculo = do
    putStrLn $ "Pregunta: " ++ pregunta oraculo
    imprimirOpciones $ opciones oraculo
    resp <- getLine
    case resp of
      "ninguna" -> do
        putStrLn "\n[!] ¡He fallado! ¿Cuál opción esperabas?"
        opcionNueva <- getLine
        putStrLn "¿Cuál es la respuesta correcta?"
        respNueva <- getLine
        -- Añadir nueva opción del usuario a la pregunta
        return (agregarOpcion oraculo (crearOraculo respNueva) opcionNueva)
      _         -> 
        if resp `Map.member` (opciones oraculo) then do
          -- Retornamos el subarbol para tener el arbol completo al final
          sub <- predecir (respuesta oraculo resp)
          return (agregarOpcion oraculo sub resp)
        else do -- Respuesta no válida
          putStrLn "\nLa respuesta no es una opción válida. Intente de nuevo."
          predecirPreg oraculo

  -- Predicción para Oráculo de tipo Predicción.
  -- Retorna un Oráculo nuevo con los cambios correspondientes al 
  -- proceso de predicción
  predecirPred :: Oraculo -> IO Oraculo
  predecirPred oraculo = do
    putStrLn $ "Predicción: " ++ prediccion oraculo
    putStrLn "si / no"
    resp <- getLine
    case resp of
      "si" -> do 
        putStrLn "\n[!] Haskinator hizo la predicción correcta.\n"
        return (oraculo)
      "no" -> do -- Ramificar (crear pregunta nueva + opción)
        putStrLn "\n[!] Haskinator se equivocó en su predicción.\n"

        putStrLn "¿Cuál es la respuesta correcta?"
        respCorrc <- getLine

        putStrLn $ "¿Qué pregunta distingue a " ++ respCorrc ++ " de las otras opciones?"
        pregDist <- getLine

        putStrLn $ "¿Cuál es la respuesta a " ++ pregDist ++ " para " ++ respCorrc ++"?"
        opcCorrc <- getLine

        putStrLn $ "¿Cuál es la respuesta a " ++ pregDist ++ " para " ++ (prediccion oraculo) ++"?"
        opcOrac <- getLine

        return (ramificar [opcCorrc, opcOrac] [crearOraculo respCorrc, oraculo] pregDist)
      _    -> do -- Respuesta no válida
        putStrLn "\n[!] Haskinator no entiende tu respuesta."
        predecirPred oraculo

  -- Recibe: PREGUNTA DONDE FALLO, PREDICCION CORRECTA, OPCION NUEVA
  -- Retorna: El ORACULO MODIFICADO CON LA NUEVA OPCION
  agregarOpcion :: Oraculo -> Oraculo -> String -> Oraculo
  agregarOpcion (Pregunta txt ops) pred opc = Pregunta txt (Map.insert opc pred ops)

  -- Muestra al usuario las opciones de la pregunta correspondiente
  imprimirOpciones :: Opciones -> IO ()
  imprimirOpciones ops = putStrLn $ (intercalate " / " (Map.keys ops)) ++ " / ninguna"

  -- Guarda en un archivo el Oráculo corresponiente y retorna al menu
  persistir :: Oraculo -> IO ()
  persistir oraculo = do
    putStrLn "Ingrese el nombre del archivo para guardar:"
    putStrLn "Ejemplo: oraculo.txt"
    file <- getLine
    writeFile file (show oraculo)
    putStrLn $ "El Oráculo ha sido guardado en " ++ file
    cicloHaskinator oraculo

  -- Carga desde un archivo la información para crear un Oráculo
  cargar :: IO Oraculo
  cargar = do
    putStrLn "Ingrese el nombre del archivo para cargar:"
    file <- getLine
    fileExists <- doesFileExist file
    if fileExists then do
      temp <- readFile file
      return (read temp :: Oraculo)
    else do
      putStrLn $ "Error: El archivo " ++ file ++ " no existe."
      putStrLn "Intente de nuevo."
      cargar 

  -- Consultas pregunta crucial de dos predicciones que deben estar
  -- presentes en el Oráculo actual
  consultar :: Oraculo -> IO ()
  consultar oraculo = do
    putStrLn "\nIngrese una predicción:"
    txt1 <- getLine
    putStrLn "Ingrese otra predicción:"
    txt2 <- getLine
    let pred1 = crearOraculo txt1
    let pred2 = crearOraculo txt2
    
    if (esAncestro pred1 pred2 oraculo && (txt1 /= txt2)) then
      imprimirCrucial (hallarOpciones (hallarLCA pred1 pred2 oraculo) pred1 pred2) txt1 txt2
    else
      predNoExiste (predExiste pred1 oraculo) (predExiste pred2 oraculo) txt1 txt2
    
    cicloHaskinator oraculo

  -- Imprime en pantalla la pregunta crucial encontrada
  imprimirCrucial :: (Oraculo, String, String) -> String -> String -> IO ()
  imprimirCrucial (preg, o1, o2) p1 p2 = do
    putStrLn $ "\nPregunta Crucial: " ++ pregunta preg
    putStrLn $ "Opción para " ++ p1 ++ ": " ++ o1
    putStrLn $ "Opción para " ++ p2 ++ ": " ++ o2

  -- Dada la pregunta, buscamos que opciones corresponden a cada predicción
  hallarOpciones :: Oraculo -> Oraculo -> Oraculo -> (Oraculo, String, String)
  hallarOpciones preg p1 p2 = (preg, foldl hallar_p1 "" hijos, foldl hallar_p2 "" hijos)
      where hallar_p1 acc (op, orac) = if predExiste p1 orac then op else acc
            hallar_p2 acc (op, orac) = if predExiste p2 orac then op else acc
            hijos = Map.toList (opciones preg)

  -- Hallar LCA buscando cuales Oraculos tienen las predicciones en sus subarboles
  hallarLCA :: Oraculo -> Oraculo -> Oraculo -> Oraculo  
  hallarLCA p1 p2 oraculo = auxLCA p1 p2 oraculo
  
  auxLCA :: Oraculo -> Oraculo -> Oraculo -> Oraculo
  auxLCA p1 p2 root = foldl (\acc (_, orac) -> if (esAncestro p1 p2 orac) then auxLCA p1 p2 orac else acc) root hijos
    where hijos = Map.toList (opciones root)

  -- Retorna si el Oraculo contiene en sus descendientes a AMBAS predicciones (es ancestro de ambas)
  esAncestro :: Oraculo -> Oraculo -> Oraculo -> Bool
  esAncestro p1 p2 oraculo = predExiste p1 oraculo && predExiste p2 oraculo

  -- Dada una predicción, revisa si se encuentra en el Oráaculo
  predExiste :: Oraculo -> Oraculo -> Bool
  predExiste pred (Prediccion txt) = txt == prediccion pred
  predExiste pred (Pregunta _ ops) = foldl (\acc (_, orac) -> predExiste pred orac || acc) False lista
    where lista = Map.toList ops

  predNoExiste :: Bool -> Bool -> String -> String -> IO ()
  predNoExiste True False p1 p2 = putStrLn $ "\n[!] La predicción " ++ p2 ++ " NO es válida."
  predNoExiste False True p1 p2 = putStrLn $ "\n[!] La predicción " ++ p1 ++ " NO es válida."
  predNoExiste False False p1 p2 = putStrLn $ "\n[!] Las predicciones " ++ p1 ++ " y " ++ p2  ++ " NO son válidas."
