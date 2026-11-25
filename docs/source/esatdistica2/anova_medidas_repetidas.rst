Anova de medidas repetidas
==========================

.. code:: Python

   import pandas as pd
   import statsmodels.api as sm
   from statsmodels.stats.anova import AnovaRM

   # 1. Crear un DataFrame de ejemplo
   # Sujetos (10 personas), medidos en 3 condiciones/tiempos (Antes, Durante, Después)
   data = {
    'sujeto': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] * 3,
    'condicion': ['antes']*10 + ['durante']*10 + ['despues']*10,
    'valor': [
        12, 14, 15, 11, 13, 12, 14, 16, 12, 13,  # antes
        15, 18, 17, 14, 16, 15, 17, 19, 14, 15,  # durante
        10, 12, 11, 9,  11, 10, 11, 13, 10, 11   # después
    ]
   }

   df = pd.DataFrame(data)

   # 2. Ejecutar el ANOVA de medidas repetidas
   # dv = variable dependiente, within = factor(es) de medidas repetidas, subject = identificador del sujeto
   anova_model = AnovaRM(
    data=df, 
    depvar='valor', 
    subject='sujeto', 
    within=['condicion']
   )

   res = anova_model.fit()
   print(res.summary())

