Nifti
==========

El formato NIfTI (Neuroimaging Informatics Technology Initiative) es un formato de archivo estándar utilizado para almacenar imágenes 
médicas multidimensionales, especialmente en neuroimagen y resonancia magnética (RM).


**Características principales**

• Extensiones: Suele guardarse como un único archivo con extensión .nii (que incluye cabecera y datos) o comprimido como .nii.gz. También admite el par .hdr (metadatos) y .img (datos de imagen).

• Dimensiones: Soporta volúmenes 3D (espacio: x, y, z) y series temporales 4D (espacio más tiempo: t), además de dimensiones adicionales para datos especializados.

• Orientación clara: Corrige limitaciones del antiguo formato Analyze al definir mejor la orientación espacial y reducir la ambigüedad izquierda-derecha.


**Ejemplos de lectura de este tipo de formato**

**1)**

.. code:: Python

   import nibabel as nib
   import matplotlib.pyplot as plt

   pat = '/Users/santosg/Datos_Nifti/'

   img = nib.load(pat+'sub-01_T1w.nii.gz')

   datos = img.get_fdata()

   print(datos.shape)

   imgS = datos[:,:,50]
   imgA = datos[:,100,:]
   img = datos[100,:,:]

   plt.imshow(img, cmap='gray')
   plt.show()

**2)**

.. code:: Python
   
   import glob
   import nibabel as nib
   import matplotlib.pyplot as plt

   pat = '/Users/santosg/Datos_Nifti/'

   img = nib.load(pat+'sub-01_T1w.nii.gz')

   datos = img.get_fdata()

   dd = datos.shape

   # nz = dd[2] axial
   ny =  dd[1]

   for i in range(ny):
     img = datos[:,i,:]

     plt.imshow(img, cmap='gray')
     plt.pause(0.2)
     print(i)

   plt.ioff()
   plt.show()


