import nibabel as nib
import matplotlib.pyplot as plt

pat = '/Users/santosg/Datos_Nifti/'

img = nib.load(pat+'sub-01_T1w.nii.gz')

#print(dir(img))

datos = img.get_fdata()

print(datos.shape)

imgS = datos[:,:,50]
imgA = datos[:,100,:]
img = datos[100,:,:]

plt.imshow(img, cmap='gray')
plt.show()

