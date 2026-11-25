tukey_hsd
=========

La prueba de diferencia honestamente significativa (HSD, por sus siglas en inglés) de Tukey realiza comparaciones de 
medias por pares para un conjunto de muestras. Mientras que el ANOVA (por ejemplo, `f_oneway`) evalúa si las medias reales 
subyacentes a cada muestra son idénticas, la prueba HSD de Tukey es un análisis **post hoc** que se utiliza para comparar 
la 
media de cada muestra con la media de cada una de las otras muestras.

**Ejemplo**

.. code:: Python

   import numpy as np
   from scipy.stats import tukey_hsd

   grupo0 = [24.5, 23.5, 26.4, 27.1, 29.9]
   grupo1 = [28.4, 34.2, 29.5, 32.2, 30.1]
   grupo2 = [26.1, 28.3, 24.3, 26.2, 27.8]

   fig, ax = plt.subplots(1, 1)
   
   plt.boxplot([grupo0, grupo1, grupo2])
   plt.show()

res = tukey_hsd(group0, group1, group2)
print(res)

result = tukey_hsd(group0, group1, group2)
conf = res.confidence_interval(confidence_level=.99)
for ((i, j), l) in np.ndenumerate(conf.low):
    # filter out self comparisons
    if i != j:
        h = conf.high[i,j]
        print(f"({i} - {j}) {l:>6.3f} {h:>6.3f}")


