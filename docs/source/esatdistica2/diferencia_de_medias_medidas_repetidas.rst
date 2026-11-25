diferencia_de_medias_medidas_repetidas
======================================

Para comparar medias en medidas repetidas en Python, la prueba depende de cuántas mediciones o condiciones tenga cada sujeto:

• 2 mediciones o condiciones (ej. antes y después): Prueba t pareada (scipy.stats.ttest_rel).
• 3 o más mediciones o condiciones (ej. línea base, mes 1, mes 3): ANOVA de medidas repetidas (statsmodels.stats.anova.AnovaRM).

**1. Dos medidas: Prueba t pareada (SciPy)**

Evalúa si la diferencia media entre dos mediciones del mismo grupo es distinta de cero.

.. code:: Python

   import numpy as np
   from scipy import stats

   # Mismos sujetos medidos antes y después
   antes = np.array([78, 82, 85, 74, 90, 79, 88, 81])
   despues = np.array([81, 84, 86, 79, 92, 80, 91, 84])

   t_stat, p_val = stats.ttest_rel(antes, despues)

   print(f"Estadístico t: {t_stat:.4f}")
   print(f"Valor p: {p_val:.4f}")
   print(f"Diferencia media (después - antes): {np.mean(despues - antes):.2f}")

• Si p_val < 0.05 (con α = 0.05), existe una diferencia estadísticamente significativa entre ambas mediciones.
• Si no se cumple la normalidad de las diferencias, usa la alternativa no paramétrica: stats.wilcoxon(antes, despues).


**2. Tres o más medidas: ANOVA de medidas repetidas (statsmodels)**

Evalúa si hay diferencias significativas entre las medias de tres o más momentos o condiciones evaluadas en los mismos sujetos. Requiere 
los datos en formato largo (una fila por observación).

.. code:: Python

   import pandas as pd
   from statsmodels.stats.anova import AnovaRM

   df = pd.DataFrame({
    "sujeto": [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5],
    "tiempo": [
        "t1",
        "t2",
        "t3",
        "t1",
        "t2",
        "t3",
        "t1",
        "t2",
        "t3",
        "t1",
        "t2",
        "t3",
        "t1",
        "t2",
        "t3",
    ],
    "puntaje": [60, 65, 70, 55, 58, 64, 72, 74, 75, 50, 56, 61, 68, 70, 73],
   })

   modelo = AnovaRM(data=df, depvar="puntaje", subject="sujeto", within=["tiempo"])
   resultados = modelo.fit()
   print(resultados)

• Revisa Pr > F en la tabla de tiempo: un valor menor a 0.05 indica que al menos una condición difiere significativamente de las demás.
• Si resulta significativo, aplica comparaciones post hoc pareadas (por ejemplo, con ttest_rel y corrección de Bonferroni) para identificar qué pares difieren.
• Si los datos no cumplen los supuestos paramétricos, usa la prueba de Friedman: stats.friedmanchisquare(t1, t2, t3).

**Supuestos clave a verificar**

1. Pareamiento correcto: Cada fila o par debe corresponder exactamente al mismo sujeto u observación.

2. Normalidad: En la prueba t pareada se evalúa en las diferencias (despues - antes), por ejemplo con stats.shapiro(despues - antes).

3. Esfericidad (en ANOVA de medidas repetidas): Supone varianzas iguales de las diferencias entre todos los pares de condiciones.

Si quieres, indícame:

• Cuántas mediciones o condiciones tiene cada sujeto
• Si tus datos son normales o si tienes datos faltantes / múltiples factores

Con eso puedo darte el código exacto de supuestos, pruebas post hoc o modelos mixtos.
