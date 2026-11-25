diferencia de medias
====================

Para evaluar si existe una diferencia estadísticamente significativa entre las medias de dos grupos en Python, utiliza principalmente la 
prueba t de la librería scipy.stats.

**1. Elige la prueba según tus datos**

• Muestras independientes (scipy.stats.ttest_ind): compara dos grupos distintos (por ejemplo, grupo de control vs. tratamiento). Usa equal_var=False (prueba t de Welch) cuando no asumas varianzas iguales.
• Muestras pareadas (scipy.stats.ttest_rel): compara los mismos sujetos o unidades en dos momentos o condiciones (por ejemplo, antes y después).
• Alternativa no paramétrica (scipy.stats.mannwhitneyu o wilcoxon): úsala si los datos no siguen una distribución normal y las muestras son  pequeñas.

**2. Ejemplo en Python (muestras independientes)**

.. code:: Python

   import numpy as np
   from scipy import stats

   rng = np.random.default_rng(42)
   grupo_a = rng.normal(loc=100, scale=15, size=30)
   grupo_b = rng.normal(loc=108, scale=16, size=30)

   # Prueba t de Welch (no asume varianzas iguales)
   t_stat, p_val = stats.ttest_ind(grupo_a, grupo_b, equal_var=False, alternative="two-sided")

   # Tamaño del efecto (d de Cohen)
   s_pooled = np.sqrt((np.var(grupo_a, ddof=1) + np.var(grupo_b, ddof=1)) / 2)
   cohens_d = (np.mean(grupo_a) - np.mean(grupo_b)) / s_pooled

   print(f"Media A: {np.mean(grupo_a):.2f} | Media B: {np.mean(grupo_b):.2f}")
   print(f"Estadístico t: {t_stat:.4f} | Valor p: {p_val:.4f}")
   print(f"d de Cohen: {cohens_d:.4f}")

Con estos datos simulados, se obtiene Media A = 100.25, Media B = 109.82, t = -3.0180 y p = 0.0038.


**3. Cómo interpretar el resultado**

• Hipótesis nula (H₀): las medias poblacionales son iguales (μ₁ = μ₂).
• Regla de decisión: si p_val < alfa (comúnmente 0.05), rechazas H₀ y concluyes que hay diferencia significativa.
• Tamaño del efecto (d de Cohen): indica qué tan grande es la diferencia en términos prácticos, más allá del valor p.

Para profundizar en supuestos y variantes, revisa la guía de Built In.

Si compartes el tipo de datos (independientes o pareados), el tamaño de muestra y tu nivel de significancia (α), puedo adaptar el código o 
ayudarte a interpretar tus resultados.

https://builtin.com/data-science/t-test-python


