{--
4) Para el tipo leftist heaps:
a – Que es una espina en un arbol? Que es el rango de una  leftist Heaps?
b – Invariante de Heaps y leftist Heaps
c – Consecuencias que tienen el/los invariantes de Ieftist Heaps
--}

{--

a – ¿Qué es una espina en un árbol? ¿Qué es el rango de una Leftist Heap?

Espina (spine):
En el contexto de Leftist Heaps, 
la espina es el camino más corto desde la raíz hasta una hoja (específicamente, siguiendo los hijos derechos).
Se usa para garantizar eficiencia en operaciones como la unión (merge).

Rango (rank):
El rango de un nodo en un Leftist Heap,
es la longitud del camino más corto desde ese nodo hasta una hoja siguiendo exclusivamente hijos derechos.
Es decir, el número de nodos en su espina derecha. Formalmente:

rango(nodo) = 0 si no tiene hijos
rango(nodo) = 1 + min (rango(hijo izquierdo), rango(hijo derecho))

Pero dado que se mantiene el invariante leftist, siempre se cumple:
rango(hijo izquierdo) ≥ rango(hijo derecho), por lo que:

rango(nodo) = 1 + rango(hijo derecho)
--}

{--
b – Invariante de Heaps y Leftist Heaps

Invariante de Leftist Heap (Leftist Property):
Para cada nodo, el rango del hijo izquierdo ≥ rango del hijo derecho.
Esto asegura que el camino derecho (espina) es el más corto, permitiendo una unión eficiente en O(log n).
--}

{--
c – Consecuencias de los invariantes en Leftist Heaps
Operación de merge eficiente:
La espina derecha es corta, por lo que al fusionar dos heaps, se sigue recursivamente la espina derecha de uno y se "pegan" los nodos del otro. Esto toma tiempo proporcional a la longitud de las espinas: O(log n).

Tiempos de operación garantizados:
Gracias a los invariantes, las operaciones como insertar, eliminar el mínimo o unir dos heaps son eficientes (logarítmicas).

Forma desequilibrada controlada:
A diferencia de un heap binario completo, los Leftist Heaps no son balanceados en forma tradicional, pero sí están estructurados para que la parte "desbalanceada" esté en la espina derecha, que es lo que importa para la eficiencia.
--}