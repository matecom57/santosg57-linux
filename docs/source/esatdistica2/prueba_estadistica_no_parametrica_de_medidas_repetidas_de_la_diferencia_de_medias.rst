prueba_estadistica_no_parametrica_de_medidas_repetidas_de_la_diferencia_de_medias

Para analizar medidas repetidas (muestras relacionadas o pareadas) cuando los datos no siguen una distribución normal, la prueba a elegir 
depende de la cantidad de momentos o condiciones que compares: la prueba de rangos con signo de Wilcoxon (para 2 medidas) o la prueba de 
Friedman (para 3 o más medidas).

(Nota: Al ser pruebas no paramétricas basadas en rangos, técnicamente evalúan diferencias en las medianas o en las distribuciones 
generales, funcionando como el equivalente directo a la comparación de medias).


**Guía rápida de elección**


Número de medidas			Equivalente Paramétrico		Prueba No Paramétrica		Función en scipy.stats

2 medidas (ej. Antes vs. Después)	Prueba t pareada		Rangos con signo de Wilcoxon	wilcoxon()

3 o más medidas (ej. Mes 1, 2 y 3)	ANOVA de medidas repetidas	Prueba de Friedman		friedmanchisquare()


**1. Para 2 medidas repetidas: Prueba de Wilcoxon**

Se utiliza para comparar dos mediciones tomadas sobre los mismos sujetos.

.. code:: Python

   from scipy.stats import wilcoxon

   # Ejemplo: Puntuación de dolor de 10 pacientes antes y después de un tratamiento
   antes = [8, 7, 9, 6, 8, 7, 9, 10, 6, 7]
   despues = [5, 4, 6, 6, 4, 3, 5, 7, 4, 5]

   # Ejecutar prueba
   estadistico, p_valor = wilcoxon(antes, despues)

   print(f"Estadístico W: {estadistico}")
   print(f"Valor p: {p_valor:.4f}")

   if p_valor < 0.05:
    print("Hay una diferencia estadísticamente significativa entre las dos mediciones.")
   else:
    print("No hay evidencia de diferencia significativa.")



**2. Para 3 o más medidas repetidas: Prueba de Friedman**

Se utiliza cuando evalúas al mismo grupo de sujetos en tres o más momentos o condiciones diferentes.

.. code:: Python

   from scipy.stats import friedmanchisquare

   # Ejemplo: Tiempo de reacción de 8 sujetos en 3 momentos del día
   manana = [250, 270, 260, 290, 240, 255, 280, 265]
   tarde  = [260, 285, 275, 300, 250, 265, 295, 270]
   noche  = [290, 310, 305, 330, 280, 295, 320, 300]

   # Ejecutar prueba
   estadistico, p_valor = friedmanchisquare(manana, tarde, noche)

   print(f"Estadístico Chi-cuadrado: {estadistico:.4f}")
   print(f"Valor p: {p_valor:.4f}")

   if p_valor < 0.05:
    print("Al menos una de las mediciones difiere significativamente de las demás.")
   else:
    print("No hay diferencias significativas entre los momentos.")


**¿Qué hacer si Friedman da significativo (p < 0.05)?**

La prueba de Friedman te indica que existe una diferencia, pero no te dice entre qué pares exactos ocurre (ej. si es entre mañana y noche). 
Para descubrirlo, se realiza una prueba post-hoc como el test de Nemenyi (disponible en la librería scikit-posthocs mediante 
sp.posthoc_nemenyi_friedman()) o pruebas de Wilcoxon por pares aplicando una corrección de Bonferroni al valor p.

Si lo necesitas, dime:

• Cuántos grupos o momentos estás comparando
• El formato de tus datos (listas, columnas de un DataFrame de Pandas, etc.)

Puedo adaptar el código e incluir el análisis post-hoc exacto para tu caso.



