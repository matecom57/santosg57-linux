Prueba anova de tres muestras o mas independientes
==================================================

.. code:: Python

   from scipy import stats

   # Datos de ejemplo de 3 grupos diferentes

   grupo_a = [23, 25, 29, 34, 30]
   grupo_b = [19, 22, 21, 24, 25]
   grupo_c = [31, 33, 35, 30, 32]

   # Aplicar ANOVA de un factor
   estadistico_f, p_valor = stats.f_oneway(grupo_a, grupo_b, grupo_c)

   print(f"Estadístico F: {estadistico_f}")
   print(f"P-valor: {p_valor}")

   if p_valor < 0.05:
       print("Hay diferencias significativas entre los grupos.")
   else:
       print("No hay diferencias significativas.")


