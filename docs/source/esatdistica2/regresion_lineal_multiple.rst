regresion lineal multiple
=========================

La regresión lineal múltiple modela una variable dependiente y a partir de dos o más variables independientes \(X_1, X_2, \dots, X_n\) 
mediante la ecuación \(y = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \cdots + \beta_n X_n\), estimando los coeficientes con mínimos cuadrados 
ordinarios en Python.

**Implementación con scikit-learn**

Usa scikit-learn cuando tu objetivo principal es entrenar un modelo predictivo y evaluarlo en datos de prueba.

.. code:: Python

   import pandas as pd
   from sklearn.linear_model import LinearRegression
   from sklearn.metrics import mean_squared_error, r2_score
   from sklearn.model_selection import train_test_split

   # 1. Preparar datos (ejemplo)
   df = pd.DataFrame(
    {
        "metros": [60, 85, 110, 140, 95, 120],
        "habitaciones": [2, 3, 3, 4, 3, 4],
        "antiguedad": [15, 10, 5, 2, 8, 6],
        "precio": [150, 210, 260, 340, 230, 290],
    }
   )

   X = df[["metros", "habitaciones", "antiguedad"]]
   y = df["precio"]

   # 2. Dividir en entrenamiento y prueba
   X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42
   )

   # 3. Ajustar el modelo
   modelo = LinearRegression()
   modelo.fit(X_train, y_train)

   # 4. Predecir y evaluar
   y_pred = modelo.predict(X_test)
   print("Intercepto (beta_0):", modelo.intercept_)
   print("Coeficientes:", dict(zip(X.columns, modelo.coef_)))
   print("R2:", r2_score(y_test, y_pred))
   print("MSE:", mean_squared_error(y_test, y_pred))

Para más ejemplos prácticos de flujo de trabajo, consulta la guía de DigitalOcean o la explicación conceptual en GeeksforGeeks.

**Diagnóstico estadístico con statsmodels**

Usa statsmodels cuando necesites valores p, intervalos de confianza, error estándar de cada coeficiente y R² ajustado para validar la 
significancia estadística.

.. code:: Python

   import statsmodels.api as sm

   # Agregar columna de unos para el intercepto
   X_sm = sm.add_constant(X)
   modelo_sm = sm.OLS(y, X_sm).fit()

   print(modelo_sm.summary())


**Buenas prácticas clave**

• Variables categóricas: Conviértelas a variables indicadoras (one-hot encoding o pd.get_dummies(..., drop_first=True)) para evitar colinealidad perfecta.
• Multicolinealidad: Revisa la correlación entre predictores y el factor de inflación de la varianza (VIF) si los coeficientes cambian mucho o pierden significancia.
• Métrica de ajuste: Compara R² y R² ajustado; este último penaliza agregar variables irrelevantes.




