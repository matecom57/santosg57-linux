prueba_estadistica-no_parametrica_de_la_media_poblacional
=========================================================

Las pruebas no paramétricas clásicas (como Wilcoxon de rangos con signo o la prueba de signos) evalúan la mediana o la localización de una 
población sin asumir normalidad, no directamente la media poblacional. Para contrastar o estimar la media poblacional sin asumir una 
distribución paramétrica en Python, se utiliza comúnmente bootstrap (remuestreo) con numpy o scipy.stats.bootstrap.

**1. Prueba de hipótesis para la media poblacional con Bootstrap**

Si quieres probar si la media poblacional (μ) es igual a un valor de referencia μ₀ (por ejemplo, μ₀ = 100) sin asumir normalidad:

.. code:: Python

   import numpy as np
   from scipy import stats

   rng = np.random.default_rng(42)
   datos = np.array([92, 95, 98, 101, 104, 107, 115, 140, 88, 99])
   mu_0 = 100

   # Centramos los datos bajo la hipótesis nula (media = mu_0)
   datos_centrados = datos - np.mean(datos) + mu_0

   # Generamos medias bootstrap bajo H0
   n_boot = 10000
   medias_boot = np.array([
    np.mean(rng.choice(datos_centrados, size=len(datos), replace=True))
    for _ in range(n_boot)
   ])

   media_obs = np.mean(datos)
   diff_obs = abs(media_obs - mu_0)

   # Valor p bilateral
   p_value = np.mean(np.abs(medias_boot - mu_0) >= diff_obs)

   print(f"Media muestral: {media_obs:.2f}")
   print(f"Valor p (bootstrap): {p_value:.4f}")
 
**2. Intervalo de confianza bootstrap para la media**

scipy.stats.bootstrap calcula intervalos de confianza no paramétricos para la media:

.. code:: Python

   import numpy as np
   from scipy import stats

   datos = np.array([92, 95, 98, 101, 104, 107, 115, 140, 88, 99])

   res = stats.bootstrap(
    (datos,),
    statistic=np.mean,
    confidence_level=0.95,
    method="BCa",
    random_state=42,
   )

   print(f"Media: {np.mean(datos):.2f}")
   print(f"IC 95% bootstrap: ({res.confidence_interval.low:.2f}, {res.confidence_interval.high:.2f})")


**3. Alternativa de localización (mediana): Wilcoxon de una muestra**

Si tu objetivo admite evaluar la mediana (localización central robusta ante valores atípicos y asimetría) en lugar de la media, puedes usar 
scipy.stats.wilcoxon:

.. code:: Python

   import numpy as np
   from scipy import stats

   datos = np.array([92, 95, 98, 101, 104, 107, 115, 140, 88, 99])
   mediana_0 = 100

   # Se resta mediana_0 para contrastar si la mediana de las diferencias es 0
   stat, p_value = stats.wilcoxon(datos - mediana_0, alternative="two-sided")
   print(f"Estadístico W: {stat}, Valor p: {p_value:.4f}")





