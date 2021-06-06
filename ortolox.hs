
-- Esta funcion convierte grados a radianes
radius = 6371

rad x = x * (pi /180)

-- Esta funcion convierte de radianes a grados
deg x = x * (180/pi)

-- Esta funcion calcula ortodromica, basado en la formula del pdf
ortodromica lon0 lat0 lon1 lat1 = do
    let lonDelta = lon1 - lon0
    let res = (sin lat0*(sin lat1) + (cos lat0)*(cos lat1)*(cos lonDelta))
    radius * (acos res)

-- Esta funcion calcula el rumbo inicial para las latitudes y longitudes
rinicial lon0 lat0 lon1 lat1 = do
    let lonDelta = lon1 - lon0
    let dividendo = (cos lat1)*(sin lonDelta)
    let divisor = ((cos lat0) * (sin lat1)) - ((sin lat0) * (cos lat1) * (cos lonDelta))
    deg (atan2 dividendo divisor)

-- Esta funcion calucla el rumbo de retorno para las latitudes y longitudes
rretorno lon0 lat0 lon1 lat1 = do
    let lonDelta = lon1 - lon0
    let dividendo = (cos lat0)*(sin lonDelta)
    let divisor = (-(cos lat1) * (sin lat0)) + ((sin lat1) * (cos lat0) * (cos lonDelta))
    -- se nego dividor y el resultado del arcotangente, porque por los signos,
    -- aun asi no es segura esta instruccion
    deg (0 - (atan2 dividendo (0-divisor)))

-- esta funcion convierte la longitud a un numero x
lx lon = radius * lon

-- esta funcion converte la latidud a un numero y
ly lat = log (tan ( (pi / 4) + (lat/2) ) )

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
loxodromica alpha lon0 lat0 lon1 lat1 = do
    let div = if lat1 == lat0 
        then abs ((lon1 - lon0)* (cos lat0))
        else abs ( (lat1 - lat0)/ (cos alpha) )
    radius * div

-- Esta es la funcion principal
-- lee la longitud y latitud de los dos paises
-- luego calcula los valores del ejercio
main = do
    city0 <- getLine
    num <- getLine
    let longitud0 = rad (read num::Double)
    num <- getLine
    let latitud0 = rad (read num::Double)

    city1 <- getLine
    num <- getLine
    let longitud1 = rad (read num::Double)
    num <- getLine
    let latitud1 = rad (read num::Double)

    putStrLn ("Ruta "++city0++"-"++city1)
    putStr "Ortodromica: "
    putStr (show (ortodromica longitud0 latitud0 longitud1 latitud1))
    putStrLn " Km"

    putStr "Rumbo: "
    putStr (show (rinicial longitud0 latitud0 longitud1 latitud1))
    putStrLn "º"

    putStr "Rumbo retorno: "
    putStr (show (rretorno longitud0 latitud0 longitud1 latitud1))
    putStrLn "º"

    -- edita esta parte
    let alpha = angle longitud0 (ly latitud0) longitud1 (ly latitud1)
    -- putStrLn (show alpha)
    putStr "Loxodromica: "
    putStr (show (loxodromica alpha longitud0 latitud0 longitud1 latitud1))
    putStrLn " Km"
