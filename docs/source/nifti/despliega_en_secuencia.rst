import glob
import nibabel as nib
import matplotlib.pyplot as plt

pat = '/Users/santosg/Datos_Nifti/'

img = nib.load(pat+'sub-01_T1w.nii.gz')

#print(dir(img))

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

