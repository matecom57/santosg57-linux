medidas repetidas en modelos mezclados
======================================

Un modelo lineal mixto (LMM) para medidas repetidas evalúa cómo cambia una variable a lo largo del tiempo o bajo distintas condiciones en los mismos sujetos, modelando tanto efectos fijos (tratamiento, tiempo, interacción) como efectos aleatorios (variabilidad entre sujetos) y la correlación entre mediciones

**1. Escenario del ejemplo**

Supón un ensayo clínico para reducir la presión arterial sistólica (PAS) en pacientes divididos en dos grupos: Fármaco A y Placebo. Se mide la PAS en tres momentos: línea base (T0), semana 4 (T1) y semana 8 (T2).

**2. Estructura de los datos (formato largo)**

En los modelos mixtos, los datos suelen organizarse en formato largo (una fila por medición y sujeto):


.. list-table:: Título de la tabla
   :widths: 50 50 50 50 
   :header-rows: 1

   * - id_paciente	
     - grupo	
     - tiempo	
     - pas
   * - 1	
     - Fármaco A	
     - T0	
     - 150
   * - 1	
     - Fármaco A	
     - T1	
     - 138
   * - 1	
     - Fármaco A	
     - T2	
     - 128
   * - 2	
     - Placebo	
     - T0	
     - 152
   * - 2	
     - Placebo	
     - T1	
     - 149
   * - 2	
     - Placebo	
     - T2	
     - 147

**3. Formulación del modelo**

De forma simplificada, el modelo se expresa como:

.. math::

   \text{PAS}_{ij}=\beta _{0}+\beta _{1}\,\text{Grupo}_{i}+\beta _{2}\,\text{Tiempo}_{j}+\beta _{3}\,(\text{Grupo}_{i}\times \text{Tiempo}_{j})+u_{0i}+\varepsilon _{ij}

• Efectos fijos (:math:`\beta`): El efecto promedio del grupo (:math:`\beta _{1}`), el cambio promedio en el tiempo (:math:`\beta _{2}`) y si la evolución difiere entre grupos (:math:`\beta _{3}`).

• Efecto aleatorio (:math:`u_{0i}`): El intercepto aleatorio por paciente, que captura que cada persona empieza con un nivel basal distinto.

• Error / correlación (:math:`\varepsilon _{ij}`): La variabilidad residual y la estructura de covarianza entre mediciones repetidas del mismo paciente.

**4. Código de ejemplo en R (lme4 / nlme)**

.. code:: R

   library(lme4)

   # Intercepto aleatorio por paciente con efectos fijos de grupo, tiempo e interacción
   modelo_mixto <- lmer(pas ~ grupo * tiempo + (1 | id_paciente), data = datos)
   summary(modelo_mixto)

**5. Ventajas frente al ANOVA de medidas repetidas**

• Datos faltantes o desbalanceados: Permite incluir pacientes que abandonan el estudio o faltan a una visita sin eliminar todo su registro (bajo el supuesto Missing at Random, MAR).

• Covarianza flexible: Modela mejor la correlación entre mediciones cercanas en el tiempo y no exige estrictamente el supuesto de esfericidad.

Si quieres adaptarlo a tu caso, dime:

• El software que utilizas (R, Python, SPSS, Stata o SAS)
• El tipo de variable respuesta (continua, binaria o de conteo)
• Tu diseño (mediciones en el tiempo, condiciones cruzadas, bloques o datos faltantes)


