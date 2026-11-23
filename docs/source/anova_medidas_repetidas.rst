ANOVA de medidas repetidas 
==========================

Un ANOVA de medidas repetidas (aov con término de error intrasujeto en R) evalúa si existen diferencias significativas en la media de una variable cuantitativa medida en los mismos sujetos en 3 o más momentos o condiciones

**Escenario del ejemplo**

Se mide la presión arterial sistólica (mmHg) en 6 pacientes en tres momentos: al inicio (base), a las 4 semanas (sem_4) y a las 8 semanas (sem_8) de un programa de ejercicio.

* **Hipótesis nula** (:math:`H_{0}`): La presión media es igual en los tres momentos (:math:`\mu_{\text{base}} = \mu_{\text{sem_4}} = \mu_{\text{sem_8}}`).

* **Hipótesis alternativa** (:math:`H_{1}`): Al menos un momento difiere en su media.

**Datos (formato ancho y largo)**

.. list-table:: Título de la tabla
   :widths: 25 25 50 50
   :header-rows: 1

   * - Sujeto
     - Base
     - Semana 4
     - Semana 8
   * - S1      
     - 120     
     - 112     
     - 105
   * - S1	
     - 120	
     - 112	
     - 105
   * - S2	
     - 135	
     - 126	
     - 118
   * - S3	
     - 118	
     - 110	
     - 102
   * - S4	
     - 142	
     - 131	
     - 122
   * - S5	
     - 128	
     - 119	
     - 112
   * - S6	
     - 132	
     - 124	
     - 115

* Medias por momento: base = 129.17, sem_4 = 120.33, sem_8 = 112.33.

**Código en R con aov()**

.. code:: R

   # 1. Crear datos en formato ancho y pasar a formato largo
   datos_ancho <- data.frame(
     sujeto = factor(paste0("S", 1:6)),
     base   = c(120, 135, 118, 142, 128, 132),
     sem_4  = c(112, 126, 110, 131, 119, 124),
     sem_8  = c(105, 118, 102, 122, 112, 115)
   )

   datos_largos <- reshape(
     datos_ancho,
     varying = c("base", "sem_4", "sem_8"),
     v.names = "presion",
     timevar = "tiempo",
     times = c("base", "sem_4", "sem_8"),
     direction = "long"
   )

   datos_largos$tiempo <- factor(datos_largos$tiempo, levels = c("base", "sem_4", "sem_8"))

   # 2. Ajustar ANOVA de medidas repetidas
   modelo_rm <- aov(presion ~ tiempo + Error(sujeto/tiempo), data = datos_largos)
   summary(modelo_rm)

   # 3. Comparaciones post-hoc por parejas (ajuste de Bonferroni)
   pairwise.t.test(datos_largos$presion, datos_largos$tiempo, paired = TRUE, p.adjust.method = "bonferroni")

**Tabla ANOVA resultante**

.. list-table:: Título de la tabla
   :widths: 100 25 50 50 50 50
   :header-rows: 1

   * - Fuente	
     - gl	
     - SC	
     - CM	
     - F	
     - p
   * - Error: sujeto	
     - 5	
     - 1038.94	
     - 207.79	
     - -
     - -
   * - Error: sujeto:tiempo (tiempo)	
     - 2	
     - 850.78	
     - 425.39	
     - 497.21	
     - < 0.001
   * - Residuales (sujeto:tiempo)	
     - 10	
     - 8.56	
     - 0.86	
     - —	
     - —

**Interpretación clave**

* **Efecto del tiempo:** Como F(2, 10) = 497.21 y p < 0.001, se rechaza :math:`H_{0}`: la presión arterial cambia significativamente a lo largo del tiempo.

* **Aislamiento del sujeto:** El estrato Error: sujeto separa la variabilidad entre pacientes del error residual, aumentando la potencia de la prueba.

* **Supuestos y limitaciones de aov()**: Se asume normalidad y esfericidad (igualdad de varianzas de las diferencias entre pares de condiciones). aov() no calcula la prueba de Mauchly ni correcciones como Greenhouse-Geisser; para ello, revisa la guía de ANOVA de medidas repetidas en R o el tutorial de ANOVA de una vía de medidas repetidas

Si quieres avanzar, indícame si necesitas:

• Revisar supuestos (prueba de Mauchly, corrección Greenhouse-Geisser o alternativa no paramétrica de Friedman)

• Añadir un factor entre grupos (ANOVA mixto, por ejemplo, comparar dos tipos de dieta a lo largo del tiempo)

• Analizar tus propios datos compartiendo variables o estructura

