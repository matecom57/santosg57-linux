Prueba t para muestras pareadas / dependientes (ttest_rel)
==========================================================

.. code:: Python

   import numpy as np
   from scipy import stats

   antes = [20.5, 21.0, 19.8, 20.2, 21.5]
   despues = [21.0, 21.8, 20.1, 21.0, 22.0]

   resultado = stats.ttest_rel(antes, despues)

   print(f"Estadístico t: {resultado.statistic:.4f}")
   print(f"Valor p (p-value): {resultado.pvalue:.4f}")



