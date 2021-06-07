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
ortodromica lon1 lat1 lon2 lat2 lon12 = do
    let res = (sin lat1*(sin lat2) + (cos lat1)*(cos lat2)*(cos lon12))
    radius * (acos res)

-- calcula el angulo inicial en radianes
alphaFrom lon1 lat1 lon2 lat2 lon12 = do
    let d1 = (cos lat2) * (sin lon12)
    let d2 = (cos lat1) * (sin lat2) - ((sin lat1) * (cos lat2) * (cos lon12))
    atan2 d1 d2

-- calucula el angulo final en radianes
alphaTo lon1 lat1 lon2 lat2 lon12 = do
    let d1 = (cos lat1) * (sin lon12)
    let d2 = ((sin lat2) * (cos lat1) * (cos lon12)) - ((cos lat2)* (sin lat1))
    atan2 d1 d2 
    --0-(atan2 d1 (0-d2)) -- sin esto saca -34.708562894092964
    -- atan2 d1 d2 es el resultado correcto

-- esta funcion calcula 
adjustAlpha x = if x < 0 then x + pi else x - pi

-- esta funcion calcula el anngulo alpha
-- https://planetcalc.com/713/
angle x0 y0 x1 y1 = do
    let d = x1 - x0
    let div = if (abs d) <= pi 
        then d 
        else if d < (0-pi)
            then d + (2 * pi)
            else d - (2 * pi)
    atan2 div (y1 - y0)

-- esta funcion calcula la distancia loxodromica
loxodromica alpha lon0 lat0 lon1 lat1 lon12 = do
    let div = if lat1 == lat0 
        then abs (lon12 * (cos lat0))
        else abs ( (lat1 - lat0)/ (cos alpha) )
    radius * div

-- esta funcion optiene alpha0 
alphaZero alpha1 lat1 = do
    let d1 = (cos lat1) * (sin alpha1)
    let d2 = sqrt (((cos alpha1)**2) + ((sin alpha1) ** 2) * ((sin lat1)**2))
    atan2 d1 d2

-- se calcula la longitud 01 mencionada en el articulo de wikipedia
long01 alpha0 sigma1 = do
    let d1 = (sin alpha0) * (sin sigma1)
    let d2 = cos sigma1
    atan2 d1 d2
-- esta funcion crea un nuevo punto intermedio basado en el valor de i
-- i esta entre 0 y 1
newpoint sigma1 sigma2 alpha0 lon0 i = do 
    let diff = sigma2 - sigma1
    let sigmamid = sigma1 + (diff * i)
    let latmid = alphaZero sigmamid alpha0
    let lonmid = (long01 alpha0 sigmamid) + lon0

    let anglemid = atan2 (tan alpha0) (cos sigmamid)
    (lonmid, latmid)

-- funcion para obtener la coordenada x de un punto
pointx (x, _) = x
-- funcion para obtener la coordeanda y de un punto
pointy (_, y) = y

-- muestra la lista de angulos entre los diferentes puntos
showSimple list str1 str2 = do
    let h = head list
    putStr (show h)
    if (length list) == 1
        then putStrLn str2
        else showSimpleComma list str1 str2
-- funcion de utilidad para mostrar comas entre los rumbos
showSimpleComma list str1 str2= do
    putStr str1
    showSimple (tail list) str1 str2

-- funcion que calcula los rumbos entre los puntos intermedios
rumbosEntrePoints p1 list = do
    let p2 = head list
    let res = angle (pointx p1) (ly (pointy p1)) (pointx p2) (ly (pointy p2))
    if (length list) == 1 
        then [res]
        else res:(rumbosEntrePoints p2 (tail list))

-- funcion que calcula las distancias entres los puntos intermedios
-- funcina calculando el angulo o rumbos usados
distanciasEntrePoints p1 list = do
    let p2 = head list
    let lon1 = pointx p1
    let lat1 = pointy p1
    let lon2 = pointx p2
    let lat2 = pointy p2
    let lon12 = lonAdjust (lon2 - lon1)
    let alpha = angle lon1 (ly lat1) lon2 (ly lat2)
    let loxo = loxodromica alpha lon1 lat1 lon2 lat2 lon12
    if (length list) == 1 
        then [loxo]
        else loxo:(distanciasEntrePoints p2 (tail list))


-- Esta es la funcion principal
-- lee la longitud y latitud de los dos paises
-- luego calcula los valores del ejercio
main = do
    city1 <- getLine
    num <- getLine
    --longitud 1
    let lon1 = rad (read num::Double)
    num <- getLine
    --latitud 1
    let lat1 = rad (read num::Double)

    city2 <- getLine
    num <- getLine
    let lon2 = rad (read num::Double)
    num <- getLine
    let lat2 = rad (read num::Double)
    num <- getLine
    let size = read num::Double
    let lon12 = lonAdjust (lon2 - lon1)

    let distOrto = ortodromica lon1 lat1 lon2 lat2 lon12
    putStrLn ("Ruta "++city1++"-"++city2)
    putStr "Ortodromica: "
    putStr (show distOrto)
    putStrLn " Km"

    let alpha1 = alphaFrom lon1 lat1 lon2 lat2 lon12
    putStr "Rumbo: "
    putStr (show (deg alpha1))
    putStrLn "º"

    let alpha2 = alphaTo lon1 lat1 lon2 lat2 lon12
    putStr "Rumbo retorno: "
    putStr (show ((deg (adjustAlpha alpha2))) )
    putStrLn "º"

    let alpha = angle lon1 (ly lat1) lon2 (ly lat2)
    let distLoxo = loxodromica alpha lon1 lat1 lon2 lat2 lon12
    let beta = angle lon2 (ly lat2) lon1 (ly lat1) 

    putStr "Loxodromica: "
    putStr (show distLoxo)
    putStrLn " Km"

    putStr "Rumbo: "
    putStr (show (deg alpha))
    putStrLn "º"

    putStr "Rumbo retorno: "
    putStr (show (deg beta))
    putStrLn "º"

    putStr "Diferencia: "
    putStr (show (abs (distOrto - distLoxo)))
    putStrLn " Km"

    putStr "Interpolando "
    putStr (show (round size))
    putStrLn " puntos en la ortodromica:"

    -- punto medio 

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
    print points
    putStrLn "Poli-Loxodromica: "
    putStr "Distancias: "
    -- esto genera los rumbos o angulos
    let alphas = rumbosEntrePoints (head points) (tail points)
    let distancias = distanciasEntrePoints (head points) (tail points)
    showSimple distancias " Km, " " Km."
    putStr "Rumbos: "
    showSimple (map deg alphas) "º," "º."
    
    putStr "Diferencia: "
    let diferencia = abs (distOrto- (foldl (+) 0 distancias))
    putStr (show diferencia)
    putStrLn " Km"

