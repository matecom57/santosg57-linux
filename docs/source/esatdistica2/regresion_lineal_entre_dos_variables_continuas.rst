regresion lineal entre dos variables continuas
=============================================

Para modelar la relación lineal entre dos variables continuas (\(X\) independiente y \(y\) dependiente) en Python se usa la regresión 
lineal simple (:math:`y = \beta_0 + \beta_1 X + \varepsilon`), comúnmente con scikit-learn, statsmodels o SciPy.

**Ejemplo práctico con scikit-learn**

.. code:: Python

   import numpy as np
   from sklearn.linear_model import LinearRegression
   from sklearn.metrics import mean_squared_error, r2_score

   # 1. Datos de ejemplo (dos variables continuas)
   X = np.array([1.0, 2.0, 3.0, 4.0, 5.0, 6.0]).reshape(-1, 1)  # Variable predictora (2D)
   y = np.array([2.1, 4.0, 5.9, 8.2, 10.1, 11.8])              # Variable objetivo (1D)

   # 2. Ajustar el modelo por mínimos cuadrados ordinarios (MCO)
   modelo = LinearRegression()
   modelo.fit(X, y)

   # 3. Parámetros estimados
   pendiente = modelo.coef_[0]
   intercepto = modelo.intercept_
   print(f"Ecuación: y = {intercepto:.2f} + {pendiente:.2f} * X")

   # 4. Predicciones y evaluación
   y_pred = modelo.predict(X)
   r2 = r2_score(y, y_pred)
   rmse = np.sqrt(mean_squared_error(y, y_pred))
   print(f"R²: {r2:.4f} | RMSE: {rmse:.4f}")


**Alternativas rápidas según tu objetivo**

• Análisis estadístico detallado (p-valor, intervalos de confianza, resumen completo): usa statsmodels.api.OLS (agregando sm.add_constant(X)).
• Cálculo directo de pendiente, intercepto y correlación de Pearson: usa scipy.stats.linregress(x, y) o numpy.polyfit(x, y, 1).
• Revisión visual previa: grafica un diagrama de dispersión (scatter plot) y la recta ajustada para verificar que la relación sea aproximadamente lineal y revisar residuos.




