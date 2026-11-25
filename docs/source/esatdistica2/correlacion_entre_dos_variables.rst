correlacion entre dos variables
===============================

Para calcular la correlación entre dos variables en Python, usa .corr() en pandas para relaciones lineales o monótonas, o 
scipy.stats.pearsonr / scipy.stats.spearmanr para obtener el coeficiente y su p-value. El coeficiente oscila entre -1 y +1, donde valores 
cercanos a +1 indican relación positiva fuerte, -1 relación negativa fuerte y 0 ausencia de relación lineal.

**Métodos en Python**

• Pearson (method="pearson"): mide relaciones lineales en variables continuas y es sensible a valores atípicos.
• Spearman (method="spearman"): mide relaciones monótonas basadas en rangos y funciona bien con datos no normales u ordinales.
• Kendall (method="kendall"): mide concordancia de rangos, útil en muestras pequeñas o con empates.

.. code:: Python

   import pandas as pd
   from scipy import stats

   df = pd.DataFrame({
    "horas_estudio": [1, 2, 3, 4, 5, 6],
    "calificacion": [55, 62, 68, 75, 84, 91],
   })

   # Con pandas (devuelve matriz 2x2)
   corr_pearson = df["horas_estudio"].corr(df["calificacion"], method="pearson")
   corr_spearman = df["horas_estudio"].corr(df["calificacion"], method="spearman")

   # Con SciPy (devuelve coeficiente y valor p)
   r, p_val = stats.pearsonr(df["horas_estudio"], df["calificacion"])
   rho, p_rho = stats.spearmanr(df["horas_estudio"], df["calificacion"])


**Buenas prácticas**

• Visualiza siempre los datos: un gráfico de dispersión revela relaciones no lineales o outliers que un solo número oculta.
• No confundas con causalidad: una correlación alta no demuestra que una variable cause a la otra.


