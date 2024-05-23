import Data.Time.Clock -- Importamos las funciones necesarias para trabajar con fechas y horas
import Data.List  -- Importamos la función find para buscar un elemento en una lista
import Data.Maybe (mapMaybe) -- Importamos la función mapMaybe para aplicar una función a los elementos de una lista y eliminar los elementos Nothing
import System.IO -- Importamos las funciones necesarias para trabajar con archivos:
import Control.Exception -- Importamos la función catch para capturar excepciones
import Control.DeepSeq(deepseq, ($!!)) -- Importamos la función deepseq para forzar la evaluación de una expresión
import Text.Printf (printf) -- Importamos la función printf para formatear cadenas de texto
import Text.Read (readMaybe) -- Importamos la función readMaybe para leer un valor de un tipo de datos

-- Definimos el tipo de datos Vehiculo con tres campos: placa, entrada y salida
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime
} deriving (Show, Read)

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    -- Verificamos si el vehículo ya está en el parqueadero ya que esto no lo hacia el codigo anterior
    if any (\v -> placa v == placaVehiculo && salida v == Nothing) parqueadero
    then error "El vehículo ya está en el parqueadero."
    else Vehiculo placaVehiculo tiempo Nothing : parqueadero -- Si no está, lo agregamos al parqueadero

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    -- Verificamos si el vehículo está en el parqueadero
    if any (\v -> placa v == placaVehiculo && salida v == Nothing) parqueadero
    then map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero -- Si está, registramos su salida
    else error "El vehículo no está en el parqueadero o ya salió." -- Si no está, lanzamos un error

-- Función para buscar un vehículo en el parqueadero por su placa
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v && isNothing (salida v)) parqueadero
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un vehículo ha estado en el parqueadero
tiempoEnParqueadero :: Vehiculo -> IO NominalDiffTime
tiempoEnParqueadero vehiculo = do
    tiempoActual <- getCurrentTime
    case salida vehiculo of
        Just tiempoSalida -> return $ diffUTCTime tiempoSalida (entrada vehiculo) -- Si el vehículo ya salió, calculamos el tiempo que estuvo en el parqueadero
        Nothing -> return $ diffUTCTime tiempoActual (entrada vehiculo) -- Si el vehículo aún está en el parqueadero, calculamos el tiempo desde su entrada hasta ahora

-- Función para guardar el estado actual del parqueadero en un archivo
guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = do
    withFile "parqueadero.txt" WriteMode (\h -> do -- para poder forzar el acceso
        hPutStrLn h (unlines (map mostrarVehiculo parqueadero))
        putStrLn "Parqueadero guardado en el archivo parqueadero.txt.")

-- Función para cargar el estado del parqueadero desde un archivo
cargarParqueadero :: IO [Vehiculo]
cargarParqueadero = do
    contenido <- withFile "parqueadero.txt" ReadMode (\h -> do
        contenido <- hGetContents h
        return $!! contenido)
    let lineas = lines contenido
    return $ mapMaybe leerVehiculo lineas

-- Función para mostrar la información de un vehículo
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo vehiculo =
    printf "Vehiculo {placa = \"%s\", entrada = %s, salida = %s}"
        (placa vehiculo)
        (show $ entrada vehiculo)
        (show $ salida vehiculo)

-- Función para leer la información de un vehículo desde una cadena de texto
leerVehiculo :: String -> Maybe Vehiculo
leerVehiculo = readMaybe

-- Función para listar los vehículos en el parqueadero
listarVehiculos :: [Vehiculo] -> IO ()
listarVehiculos parqueadero = do
    putStrLn "Vehículos en el parqueadero:"
    mapM_ (putStrLn . mostrarVehiculo) parqueadero

-- Función principal del programa
main :: IO ()
main = do
    parqueadero <- cargarParqueadero -- Cargamos el estado del parqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"
    cicloPrincipal parqueadero -- Iniciamos el ciclo principal del programa

-- Ciclo principal del programa
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1 . Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Listar vehículos"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
            guardarParqueadero parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado

        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarSalida placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " salido del parqueadero."
            guardarParqueadero parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado

        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    tiempoTotal <- tiempoEnParqueadero vehiculo
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal parqueadero

        "4" -> do
            listarVehiculos parqueadero
            cicloPrincipal parqueadero

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal parqueadero