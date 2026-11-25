Algunas pruebas estadísticas
============================

**Datos 1**

.. code:: Python

   [48, 35, 46, 44, 43, 42, 39, 44, 49, 49, 44, 39, 38, 49, 49, 53, 56, 57, 51, 61, 53, 66, 71, 75,
    72, 65, 67, 38, 37, 46, 44, 44, 48, 49, 30, 45, 47, 45, 48, 47, 47, 44, 48, 43, 45, 40, 48, 49,
    38, 44, 43, 47, 46, 57, 52, 54, 56, 53, 64, 53, 58, 54, 59, 56, 62, 50, 64, 53, 61, 53, 62, 57,
    52, 54, 61, 59, 57, 52, 54, 53, 62, 52, 62, 57, 59, 59, 56, 57, 53, 59, 61, 55, 61, 56, 52, 54,
    51, 50, 50, 55, 63, 50, 59, 54, 60, 50, 56, 68, 66, 71, 82, 68, 78, 66, 70, 66, 78, 69, 71, 69,
    78, 66, 68, 71, 69, 77, 76, 71, 43, 47, 48, 37, 40, 42, 38, 49, 43, 46, 34, 46, 46, 48, 47, 43,
    52, 53, 61, 60, 53, 53, 50, 53, 54, 61, 61, 61, 64, 53, 53, 54, 61, 60, 51, 50, 53, 64, 64, 53,
    60, 54, 55, 58, 62, 62, 54, 53, 61, 54, 51, 62, 57, 50, 64, 63, 65, 71, 71, 73, 66]

Chequemos si estos datos definen una distribución normal.

Utilizando la "Prueba de Shapiro-Wilk"

.. code:: Python

   mport numpy as np
   from scipy import stats

   datos = 

   # Aplicar la prueba de Shapiro-Wilk
   estadistico, p_valor = stats.shapiro(datos)

   print(f"Estadístico W: {estadistico:.4f}")
   print(f"Valor p: {p_valor:.4f}")

   # Evaluar la hipótesis nula (alfa = 0.05)
   alfa = 0.05
   if p_valor > alfa:
       print("Los datos parecen seguir una distribución normal (no se rechaza H0)")
   else:
       print("Los datos NO siguen una distribución normal (se rechaza H0)")


**Toma una muestra**

.. code:: Python

   import numpy as np

   poblacion = np.array([10, 20, 30, 40, 50, 60, 70, 80, 90, 100])

   # Muestra de 4 elementos sin reemplazo
   muestra_sin_reemplazo = np.random.choice(poblacion, size=4, replace=False)

   # Muestra con reemplazo (un elemento puede repetirse)
   muestra_con_reemplazo = np.random.choice(poblacion, size=4, replace=True)

   print("Sin reemplazo:", muestra_sin_reemplazo)
   print("Con reemplazo:", muestra_con_reemplazo)

**Datos 2**

.. code:: Python

   [96, 93, 77, 71, 80, 96, 94, 76, 85, 91, 80
    81, 72, 94, 97, 73, 95, 87, 100, 74, 89, 80
    94, 99, 73, 83, 71, 90, 99, 81, 77, 91, 80
    90, 80, 71, 94, 80, 83, 76, 77, 97, 83, 90
    94, 90, 84, 74, 80, 70, 82, 92, 84, 99, 71
    72, 85, 84, 93, 99, 78, 71, 94, 71, 91, 79
    75, 72, 94, 93, 85, 99, 84, 89, 75, 76, 95
    85, 78, 97, 71, 87, 92, 82, 88, 91, 85, 86
    85, 75, 83, 81, 70, 87, 92, 90, 96, 94, 71,  86]

