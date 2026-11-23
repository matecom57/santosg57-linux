Ejemplos del modulo seaborm
===========================

**001**

.. code:: Python
 
   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.barplot(df, x="island", y="body_mass_g")

   plt.show()

**002**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.barplot(df, x="body_mass_g", y="island", hue="island", legend=False)

   plt.show()

**003**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.barplot(df, x="island", y="body_mass_g", hue="sex")

   plt.show()

**004**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('titanic')

   sns.boxplot(x=df["age"])

   plt.show()

**005**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('titanic')

   sns.boxplot(data=df, x="age", y="class")

   plt.show()

**006**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('titanic')

   sns.boxplot(data=df, x="class", y="age", hue="alive")

   plt.show()

**007**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('titanic')

   sns.boxplot(data=df, x="class", y="age", hue="alive", fill=False, gap=.1)

   plt.show()

**008**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('titanic')

   sns.boxplot(data=df, x="age", y="deck", width=.5)

   plt.show()

**009**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   iris = sns.load_dataset("iris")
   species = iris.pop("species")
   sns.clustermap(iris)

   plt.show()

**010**

.. code:: Python

   import numpy as np
   import pandas as pd
   import seaborn as sns
   import matplotlib.pyplot as plt

   N = 6
   df = pd.DataFrame(np.random.random((N, N)))
   sns.clustermap(data=df, linewidths=5, figsize=(3, 3))

   plt.show()

**011**

.. code:: Python

   import numpy as np
   import pandas as pd
   import seaborn as sns
   import matplotlib.pyplot as plt

   np.random.seed(2)
   data = np.random.rand(6, 6)

   sns.clustermap(data)

   plt.show()

**012**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   glue = sns.load_dataset("glue").pivot(index="Model", columns="Task", values="Score")
   sns.heatmap(glue)

   plt.show()

**013**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   glue = sns.load_dataset("glue").pivot(index="Model", columns="Task", values="Score")
   sns.heatmap(glue, annot=True, fmt=".1f")

   plt.show()

**014**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   glue = sns.load_dataset("glue").pivot(index="Model", columns="Task", values="Score")
   sns.heatmap(glue, cmap=sns.cubehelix_palette(as_cmap=True))

   plt.show()

**015**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.histplot(data=df, x="flipper_length_mm")

   plt.show()

**016**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.histplot(data=df, x="flipper_length_mm", kde=True)

   plt.show()

**017**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.histplot(data=df, x="flipper_length_mm", hue="species")

   plt.show()

**018**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.lmplot(data=df, x="bill_length_mm", y="bill_depth_mm")

   plt.show()

**019**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.lmplot(data=df, x="bill_length_mm", y="bill_depth_mm", hue="species")

   plt.show()

**020**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.lmplot(
       data=df, x="bill_length_mm", y="bill_depth_mm",
       hue="species", col="sex", height=4,
   )

   plt.show()

**021**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.pairplot(df)

   plt.show()

**022**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.pairplot(df, hue="species")

   plt.show()

**023**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('penguins')

   sns.pairplot(df, kind="kde")

   plt.show()

**024**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('tips')

   sns.scatterplot(data=df, x="total_bill", y="tip", hue="time")

   plt.show()


**025**

.. code:: Python

   import seaborn as sns
   import matplotlib.pyplot as plt

   df = sns.load_dataset('tips')

   sns.scatterplot(data=df, x="total_bill", y="tip", hue="day", style="time")
 
   plt.show()
