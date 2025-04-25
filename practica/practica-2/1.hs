{--
El modelo de color RGB es un modelo aditivo que tiene al rojo, verde y azul como colores primarios.

Cualquier otro color se expresa en términos de las proporciones de
estos tres colores que es necesario combinar en forma aditiva para obtenerlo.

Dichas proporciones caracterizan a cada color de manera biunívoca, 
por lo que usualmente se utilizan estos valores como representación de un color.

Definir un tipo Color en este modelo y una funcion mezclar que permita obtener el promedio 
componente a componente entre dos colores.
--}
type Color = (Float, Float, Float)

getPromedio::Float -> Float -> Float
getPromedio x y = ((x+y) / 2)

mezclar:: Color -> Color -> Color
mezclar (r1, g1, b1) (r2, g2, b2) =
    let pr = getPromedio r1 r2
        pg = getPromedio g1 g2
        pb = getPromedio b1 b2
    in (pr, pg, pb)