prueba_estadistica-no_parametrica_de_la_diferencia_de_medias
===========================================================


Para comparar dos grupos cuando los datos no siguen una distribución normal o tienen valores atípicos, se usan pruebas no paramétricas 
basadas en rangos en lugar de la media aritmética (que suele compararse con medianas o distribuciones). Las principales en Python con 
scipy.stats son la prueba U de Mann-Whitney (muestras independientes) y la prueba de rangos con signo de Wilcoxon (muestras pareadas).

**Pruebas no paramétricas para dos grupos**

• Muestras independientes (mannwhitneyu): Alternativa no paramétrica a la prueba t para dos muestras independientes. Evalúa si una muestra tiende a tener valores mayores que la otra.
• Muestras pareadas (wilcoxon): Alternativa no paramétrica a la prueba t pareada. Compara dos mediciones relacionadas sobre los mismos sujetos o unidades (por ejemplo, antes y después).

.. code:: Python

   import numpy as np
   from scipy import stats

   # 1. Muestras independientes (U de Mann-Whitney)
   grupo_a = np.array([12, 15, 14, 10, 95, 13, 16])  # Con valor atípico (95)
   grupo_b = np.array([22, 25, 21, 24, 20, 23, 26])

   u_stat, p_val_u = stats.mannwhitneyu(grupo_a, grupo_b, alternative="two-sided")
   print(f"Mann-Whitney U -> estadístico: {u_stat:.4f}, p-valor: {p_val_u:.4f}")

   # 2. Muestras pareadas (Rangos con signo de Wilcoxon)
   antes = np.array([120, 135, 128, 140, 130, 125])
   despues = np.array([115, 130, 129, 132, 126, 120])

   w_stat, p_val_w = stats.wilcoxon(antes, despues, alternative="two-sided")
   print(f"Wilcoxon -> estadístico: {w_stat:.4f}, p-valor: {p_val_w:.4f}")


**Interpretación del resultado**

Con un nivel de significancia habitual de α = 0.05:

• Si p-valor < 0.05: Se rechaza la hipótesis nula y se concluye que existe una diferencia estadísticamente significativa entre los grupos.
• Si p-valor >= 0.05: No hay evidencia suficiente para afirmar que exista una diferencia significativa.

**¿Y si buscas específicamente la diferencia de medias sin supuestos?**

Si tu objetivo estricto es comparar medias (y no rangos/medianas) con datos no normales, puedes usar bootstrap (remuestreo) para estimar el 
intervalo de confianza o el valor p de la diferencia de medias (np.mean(muestra1) - np.mean(muestra2)). Si tienes más de dos grupos 
independientes, la alternativa no paramétrica es la prueba de Kruskal-Wallis (scipy.stats.kruskal).





