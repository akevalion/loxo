import System.Environment  
import System.IO
-- Esta funcion convierte grados a radianes
radius = 6371

rad x = x * (pi /180)

-- Esta funcion convierte de radianes a grados
deg x = x * (180/pi)

-- esta funcion convierte la longitud a un numero x
lx lon = radius * lon

-- esta funcion converte la latidud a un numero y
ly lat = log (tan ( (pi / 4) + (lat/2) ) )

-- esta funciona ajusta la diferencia de las longitudes
lonAdjust x = if (abs x) <= pi
    then x
    else if x > pi
        then x - (2 * pi)
        else x + (2 * pi)

-- Esta funcion calcula la distancia ortodromica
-- , basado en la formula del pdf
ortodromica lon1 lat1 lon2 lat2 lon12 = radius * (acos ((sin lat1*(sin lat2) + (cos lat1)*(cos lat2)*(cos lon12))))

-- calcula el angulo inicial en radianes
alphaFrom lon1 lat1 lon2 lat2 lon12 = atan2 ((cos lat2) * (sin lon12)) ((cos lat1) * (sin lat2) - ((sin lat1) * (cos lat2) * (cos lon12)))

-- calucula el angulo final en radianes
alphaTo lon1 lat1 lon2 lat2 lon12 = atan2 ((cos lat1) * (sin lon12)) (((sin lat2) * (cos lat1) * (cos lon12)) - ((cos lat2)* (sin lat1)))
    --0-(atan2 d1 (0-d2)) -- sin esto saca -34.708562894092964
    -- atan2 d1 d2 es el resultado correcto

-- esta funcion ajusta alpha
adjustAlpha x = if x < 0 then x + pi else x - pi

-- esta funcion ajusta el divisor para la funcion angle
divisorAlpha d =  if (abs d) <= pi 
    then d 
    else if d < (0-pi)
        then d + (2 * pi)
        else d - (2 * pi)

-- esta funcion calcula el anngulo alpha
-- https://planetcalc.com/713/
angle x0 y0 x1 y1 = atan2 (divisorAlpha (x1-x0)) (y1 - y0)

-- esta funcion calcula la distancia loxodromica
loxodromica alpha lon0 lat0 lon1 lat1 lon12 = radius * (if lat1 == lat0 
    then abs (lon12 * (cos lat0))
    else abs ( (lat1 - lat0)/ (cos alpha) ))

-- esta funcion optiene alpha0 
alphaZero alpha1 lat1 = atan2 ((cos lat1) * (sin alpha1)) (sqrt (((cos alpha1)**2) + ((sin alpha1) ** 2) * ((sin lat1)**2)))

-- se calcula la longitud 01 mencionada en el articulo de wikipedia
long01 alpha0 sigma1 = atan2 ((sin alpha0) * (sin sigma1)) (cos sigma1)

-- esta funcion crea un nuevo punto intermedio basado en el valor de i
-- i esta entre 0 y 1
newpoint sigma1 sigma2 alpha0 lon0 i = newpoint2 (sigmamid sigma1 sigma2 i) alpha0 lon0

-- sigmamid calcula el sigma medio en un porcentaje i
sigmamid sigma1 sigma2 i = sigma1 + ((sigma2 - sigma1) * i)

-- funcion auxiliar para crear el nuevo punto
newpoint2 sigma alpha0 lon0 = ((lonAdjust ((long01 alpha0 sigma)+lon0)), (alphaZero sigma alpha0) )

-- funcion para obtener la coordenada x de un punto
pointx (x, _) = x

-- funcion para obtener la coordeanda y de un punto
pointy (_, y) = y

-- muestra la lista de angulos entre los diferentes puntos
showSimple list str1 str2 = (show (head list)) ++ (if (length list) == 1
    then str2++"\n"
    else showSimpleComma list str1 str2)

-- funcion de utilidad para mostrar comas entre los rumbos
-- aca es necesario el do por el putStr
showSimpleComma list str1 str2 = str1 ++ (showSimple (tail list) str1 str2)

-- funcion que calcula los rumbos entre los puntos intermedios
rumbosEntrePoints p1 list = rumbosEntrePoints2 list (angle (pointx p1) (ly (pointy p1)) (pointx (head list)) (ly (pointy (head list))))

-- funcion auxiliar de rumbosEntrePoints
rumbosEntrePoints2 list res = if (length list) == 1
    then [res]
    else res:(rumbosEntrePoints (head list) (tail list))

-- funcion que calcula las distancias entres los puntos intermedios
-- funciona calculando el angulo o rumbos usados
distanciasEntrePoints p1 list = distanciasEntrePoints2 (calculateLoxo (pointx p1) (pointy p1) (pointx (head list)) (pointy (head list))) list
-- metodo auxilair para calcular la distancia loxodormica entre los puntos
calculateLoxo lon1 lat1 lon2 lat2 = loxodromica (angle lon1 (ly lat1) lon2 (ly lat2)) lon1 lat1 lon2 lat2 (lonAdjust (lon2 - lon1))
-- funcion auxiliar

distanciasEntrePoints2 loxo list = if (length list) == 1 
    then [loxo]
    else loxo:(distanciasEntrePoints (head list) (tail list))


-- Esta es la funcion principal
-- lee la longitud y latitud de los dos paises
-- luego calcula los valores del ejercio
main :: IO ()
main = do
    args <- getArgs
    if (length args) == 0
        then putStrLn "Debe ejecutar el programa con un argumento, ejemplo:\nortolox.exe scs.txt"
        else do
    fileReference <- openFile (head args) ReadMode
    
    city1 <- hGetLine fileReference
    num <- hGetLine fileReference
    --longitud 1
    let lon1 = rad (read num::Double)
    num <- hGetLine fileReference
    --latitud 1
    let lat1 = rad (read num::Double)

    city2 <- hGetLine fileReference
    num <- hGetLine fileReference
    let lon2 = rad (read num::Double)
    num <- hGetLine fileReference
    let lat2 = rad (read num::Double)
    num <- hGetLine fileReference
    let size = read num::Double
    let lon12 = lonAdjust (lon2 - lon1)
    hClose fileReference

    let sRutaDesdeHasta = "Ruta "++city1++"-"++city2++"\n"
    putStr sRutaDesdeHasta

    let distOrto = ortodromica lon1 lat1 lon2 lat2 lon12
    let sOrtodromica = "Ortodromica: "++(show distOrto)++" Km\n"
    putStr sOrtodromica

    let alpha1 = alphaFrom lon1 lat1 lon2 lat2 lon12
    let sRumbo = "Rumbo: "++(show (deg alpha1))++"º\n"
    putStr sRumbo

    let alpha2 = alphaTo lon1 lat1 lon2 lat2 lon12
    let sRumboRetorno = "Rumbo retorno: "++(show ((deg (adjustAlpha alpha2))))++"º\n"
    putStr sRumboRetorno

    let alpha = angle lon1 (ly lat1) lon2 (ly lat2)
    let distLoxo = loxodromica alpha lon1 lat1 lon2 lat2 lon12
    let beta = angle lon2 (ly lat2) lon1 (ly lat1) 

    let sLoxodromica = "Loxodromica: "++(show distLoxo)++" Km\n"
    putStr sLoxodromica

    let sRumboLoxo = "Rumbo: "++(show (deg alpha))++"º\n"
    putStr sRumboLoxo

    let sRumboRetornoLoxo = "Rumbo retorno: "++(show (deg beta))++"º\n"
    putStr sRumboRetornoLoxo

    let sDiferencia = "Diferencia: "++(show (abs (distOrto - distLoxo)))++" Km\n"
    putStr sDiferencia

    let sInterpolando = "Interpolando "++(show (round size))++" puntos en la ortodromica:\n"
    putStr sInterpolando

    let alpha0 = alphaZero alpha1 lat1
    let sigma1 = atan2 (tan lat1) (cos alpha1)
    let sigma2 = atan2 (tan lat2) (cos alpha2)

    let lon01 = long01 alpha0 sigma1
    let lon0 = lon1 - lon01

    -- se calcula el incremento en el caso de 3, sera (1/ 4) = 0.25
    -- es decir [0, 0.25, 0.5, 0.75, 1], los valores necesarios para interpolar
    -- entre sigma1 y sigma2
    let inc = 1 / (size + 1)
    let scales = [0, inc .. 1]
    -- esto genera los nuevos puntos de control, con los angulos
    let points = map (newpoint sigma1 sigma2 alpha0 lon0) scales
    putStr (show points)
    -- esto genera los rumbos o angulos
    let alphas = rumbosEntrePoints (head points) (tail points)
    let distancias = distanciasEntrePoints (head points) (tail points)

    let sDistancias = "\nPoli-Loxodromica:\nDistancias: "++(showSimple distancias " Km, " " Km.")
    putStr sDistancias
    let sRumbos = "Rumbos: "++(showSimple (map deg alphas) "º," "º.")
    putStr sRumbos

    let diferencia = abs (distOrto- (foldl (+) 0 distancias))
    let sDiferenciaFinal = "Diferencia: "++(show diferencia)++" Km\n"
    putStr sDiferenciaFinal

    let contents = (sRutaDesdeHasta++sOrtodromica++sRumbo++sRumboRetorno ++sLoxodromica++sRumboLoxo++sRumboRetornoLoxo++sDiferencia++sInterpolando ++(show points)++sDistancias++sRumbos++sDiferenciaFinal)
    --aca se escribe el contenido del archivo que estara en codificacion ansi motivo por el cual los simbolos de los angulos no se leen correctamente
    --setLocaleEncoding =<< getFileSystemEncoding
    writeFile "resultados.txt" contents
    

