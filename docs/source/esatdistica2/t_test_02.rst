Prueba t para dos muestras independientes (ttest_ind)
=====================================================

.. code:: Python

   import matplotlib.pyplot as plt
   import numpy as np
   from scipy import stats

   grupo1 = [20.5, 21.0, 19.8, 20.2, 21.5]
   grupo2 = [18.2, 19.0, 18.5, 17.8, 19.1]

   # Realizar la prueba t independiente (equal_var=True si las varianzas son iguales, o False para Welch's t-test)

   resultado = stats.ttest_ind(grupo1, grupo2, equal_var=True)

   print(f"Estadístico t: {resultado.statistic:.4f}")
   print(f"Valor p (p-value): {resultado.pvalue:.4f}")

   plt.subplot(1, 1, 1)
   plt.boxplot([grupo1, grupo2])
   plt.show()

