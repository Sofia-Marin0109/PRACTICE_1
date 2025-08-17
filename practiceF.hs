import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception (try, catch, IOException, evaluate)
import Control.Concurrent (threadDelay)
import System.Directory (doesFileExist)


asegurarArchivo :: IO ()
asegurarArchivo = do
    existe <- doesFileExist "libreria.txt"
    if not existe
        then writeFile "libreria.txt"  ""   -- crea archivo vacío
        else return ()

readFileStrict :: FilePath -> IO String
readFileStrict fp =
  withFile fp ReadMode $ \h -> do
    contents <- hGetContents h
    _ <- evaluate (length contents)  -- fuerza a leer TODO aquí
    return contents

-- Definición del tipo de datos para representar la información 
data Libro = Libro {
    id_ :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el libre esta en préstamo o ya se devolvió
} deriving (Show, Read)

-- Función para registrar la devolución de un libro
registrarEntrada :: String -> UTCTime -> [Libro] -> [Libro]
registrarEntrada id_Libro tiempo prestamo =
    Libro id_Libro tiempo Nothing : prestamo

-- Función para registrar el préstamo de un libro
registrarSalida :: String -> UTCTime -> [Libro] -> [Libro]
registrarSalida id_Libro tiempo prestamo =
    map (\v -> if id_Libro == id_ v then v { salida = Just tiempo } else v) prestamo

-- Función para buscar un libro por su ID en el registro de préstamos
buscarLibro :: String -> [Libro] -> Maybe Libro
buscarLibro id_Libro prestamo =
    find (\v -> id_Libro == id_ v && isNothing (salida v)) prestamo
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un libro lleva en préstamo
tiempoEnPrestamo :: Libro -> UTCTime -> NominalDiffTime
tiempoEnPrestamo libro tiempoActual =
    case salida libro of
        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada libro)
        Nothing           -> diffUTCTime tiempoActual (entrada libro)

-- Función para guardar la información de los libros en un archivo de texto
guardarPrestamo :: [Libro] -> IO ()
guardarPrestamo prestamo = do
    resultado <- reintentar 5 $ (withFile "libreria.txt" WriteMode $ \h ->
        hPutStr h (unlines (map show prestamo)))
    case resultado of
        Left ex -> putStrLn $ "Error guardando el libro: " ++ show ex
        Right _ -> putStrLn "Libro guardado en el archivo libreria.txt."

-- Función para reintentar una operación en caso de error
reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
reintentar n accion = do
    resultado <- catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
    case resultado of
        Left ex -> do
            threadDelay 1000000  -- Esperar 1 segundo antes de reintentar
            reintentar (n - 1) accion
        Right val -> return (Right val)

-- Función para cargar la información de los libros desde el archivo de texto
cargarPrestamo :: IO [Libro]
cargarPrestamo = do
    asegurarArchivo
    resultado <- try (readFileStrict "libreria.txt") :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Error cargando el libro: " ++ show ex
            return []
        Right contenido -> do
            let lineas = lines contenido
            return (map read lineas)


-- Función para mostrar la información de un libro como cadena de texto
mostrarLibro :: Libro -> String
mostrarLibro libro =
    id_ libro ++ "," ++ show (entrada libro) ++ "," ++ show (salida libro)


-- Función para el ciclo principal del programa
cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal prestamo = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar préstamo de libro"
    putStrLn "2. Registrar devolución de libro"
    putStrLn "3. Buscar libro por ID"
    putStrLn "4. Listar los libros"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del libro:"
            id_Libro <- getLine
            tiempoActual <- getCurrentTime
            let prestamoActualizado = registrarEntrada id_Libro tiempoActual prestamo
            putStrLn $ "Libro con ID " ++ id_Libro ++ " ingresado al registro."
            guardarPrestamo prestamoActualizado
            cicloPrincipal prestamoActualizado

        "2" -> do
            putStrLn "Ingrese el ID del libro a devolver:"
            id_Libro <- getLine
            case buscarLibro id_Libro prestamo of
                Nothing -> do
                     putStrLn "Ese ID no se encuentra entre los libros prestados, no se puede realizar su devolución"
                     cicloPrincipal prestamo
                Just _ -> do
                      tiempoActual <- getCurrentTime
                      let prestamoActualizado = registrarSalida id_Libro tiempoActual prestamo
                      putStrLn $ "Libro con ID " ++ id_Libro ++ " ha sido devuelto."
                      guardarPrestamo prestamoActualizado
                      cicloPrincipal prestamoActualizado

        "3" -> do
            putStrLn "Ingrese el ID del libro a buscar:"
            id_Libro <- getLine
            case buscarLibro id_Libro prestamo of
                Just libro -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnPrestamo libro tiempoActual
                    putStrLn $ "El libro con ID " ++ id_Libro ++ " se encuentra en préstamo."
                    putStrLn $ "Tiempo en préstamo: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "El libro no se encuentra en préstamo."
            cicloPrincipal prestamo
        "4" -> do
            putStrLn "Mostrando Lista de libros dentro del registro"
            -- Leer la libreria actualizada
            prestamoActualizado <- cargarPrestamo
            mapM_ (\v -> putStrLn $ "ID: " ++ id_ v ++ ", Entrada: " ++ show (entrada v) ++ ", Salida: " ++ show (salida v)) prestamoActualizado
            cicloPrincipal prestamoActualizado  -- Mantenemos el archivo .txt actualizado

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal prestamo

-- Función principal del programa
main :: IO ()
main = do
    asegurarArchivo
    prestamo <- cargarPrestamo
    putStrLn "¡Bienvenido al Sistema de Gestión de prestámo y devolución de libros!"

    -- Ciclo principal del programa
    cicloPrincipal prestamo

