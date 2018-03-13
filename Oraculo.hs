-- Universidad Simón Bolívar
-- Laboratorio de Lenguajes de Programación I
-- Proyecto 1 - Haskinator
-- Autor: Adolfo Jeritson. 12-10523

module Oraculo (Oraculo(..),
                Opciones,
                crearOraculo,
                prediccion, 
                pregunta,
                opciones,
                respuesta,
                ramificar
                ) where

  import qualified Data.Map as Map

-- Definición de datos
  type Opciones = Map.Map String Oraculo
  data Oraculo = Pregunta String Opciones | Prediccion String
    deriving (Show, Read)

-- Crear
  crearOraculo :: String -> Oraculo
  crearOraculo txt = Prediccion txt

-- Accesar
  prediccion :: Oraculo -> String
  prediccion (Prediccion txt) = txt
  prediccion _ = error "El oráculo no es una predicción"

  pregunta :: Oraculo -> String
  pregunta (Pregunta txt _) = txt
  pregunta _ = error "El oráculo no es una pregunta"

  opciones :: Oraculo -> Opciones
  opciones (Pregunta _ ops) = ops
  opciones _ = error "El oráculo no es una pregunta"

  respuesta :: Oraculo -> String -> Oraculo
  respuesta (Pregunta _ ops) resp = ops Map.! resp
  respuesta _ _ = error "El oráculo no es una pregunta"
  
-- Modificar
  ramificar :: [String] -> [Oraculo] -> String -> Oraculo
  ramificar strs oracs txt = Pregunta txt (Map.fromList (zip strs oracs))
