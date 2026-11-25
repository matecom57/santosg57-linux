Shapiro-Wilk 
============

Para realizar la prueba de normalidad de Shapiro-Wilk en Python, usa la función scipy.stats.shapiro con un arreglo o lista de al menos 3 
observaciones.

**Ejemplo básico**

.. code:: Python

   import numpy as np
   from scipy import stats

   rng = np.random.default_rng(42)
   datos = rng.normal(loc=0, scale=1, size=100)

   estadistico, p_valor = stats.shapiro(datos)

   print(f"Estadístico W: {estadistico:.4f}")
   print(f"Valor p: {p_valor:.4f}")

**Cómo interpretar el resultado**

• Hipótesis nula (H₀): Los datos provienen de una distribución normal.
• Si p_valor <= 0.05 (con α = 0.05): Se rechaza H₀; hay evidencia de que los datos no siguen una distribución normal.
• Si p_valor > 0.05: No se rechaza H₀; no hay evidencia suficiente para descartar la normalidad (en muestras muy pequeñas puede faltar potencia estadística).

**Puntos clave a considerar**

• Tamaño de muestra: Funciona bien en muestras pequeñas y medianas; en muestras muy grandes, desviaciones mínimas de la normalidad pueden arrojar valores p pequeños.
• Complemento visual: Combínala con un histograma o un gráfico Q-Q para evaluar mejor la distribución.
• Alternativas si no es normal: Puedes aplicar transformaciones (como logarítmica) o usar pruebas no paramétricas según tu análisis.

Si compartes tu caso de uso (por ejemplo, validar supuestos para una prueba t o ANOVA), el tamaño de tu muestra o el resultado obtenido, 
puedo ayudarte a interpretarlo o elegir el siguiente paso.


