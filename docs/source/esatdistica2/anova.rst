ANOVA
=====

Para realizar un análisis de varianza (ANOVA) en Python, utiliza scipy.stats.f_oneway() para un ANOVA de una vía rápido o statsmodels para 
obtener tablas ANOVA detalladas y modelos multifactoriales.

**ANOVA de una vía con SciPy**

Compara si las medias de tres o más grupos son estadísticamente iguales (\(H_0: \mu_1 = \mu_2 = \dots = \mu_k\)):

.. code:: Python

   from scipy import stats

   grupo_a = [85, 88, 90, 87, 86]
   grupo_b = [78, 80, 79, 82, 77]
   grupo_c = [92, 95, 91, 94, 93]

   f_stat, p_val = stats.f_oneway(grupo_a, grupo_b, grupo_c)
   print(f"Estadístico F: {f_stat:.4f}, valor p: {p_val:.4f}")


Consulta la documentación oficial de f_oneway para revisar parámetros como equal_var o manejo de valores faltantes. Si p_val < 0.05, 
rechazas la hipótesis nula y concluyes que al menos un grupo difiere.

**Tabla ANOVA y dos vías con Statsmodels**

Cuando necesitas sumas de cuadrados, grados de libertad o evaluar dos factores y su interacción, usa statsmodels:

.. code:: Python

   import pandas as pd
   import statsmodels.api as sm
   from statsmodels.formula.api import ols

   df = pd.DataFrame({
    "puntaje": [85, 88, 90, 78, 80, 79, 92, 95, 91, 84, 86, 89],
    "metodo": ["A", "A", "A", "B", "B", "B", "A", "A", "A", "B", "B", "B"],
    "turno": ["M", "M", "M", "M", "M", "M", "T", "T", "T", "T", "T", "T"],
   })

   modelo = ols("puntaje ~ C(metodo) + C(turno) + C(metodo):C(turno)", data=df).fit()
   tabla_anova = sm.stats.anova_lm(modelo, typ=2)
   print(tabla_anova)

• Usa typ=2 o typ=3 en anova_lm() cuando tengas tamaños de muestra desiguales o interacciones.
• Verifica los supuestos clave antes de interpretar: independencia, normalidad de los residuos y homogeneidad de varianzas.



