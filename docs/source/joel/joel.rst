/Users/santosg/santosg57-hiram/source/programas
===============================================

* Filtro_media.py
* analisis.py
* copilaP.sh
* cortar_video_FFmpeg.py
* crea_imagen_total.py
* despliega_dos_imagen_y_corta_analiza.py
* despliega_imagenes.py
* despliega_serie_imagenes.py
* despliega_una_imagen.py
* despliega_una_imagen_y_corta.py
* detectar_pelicula.py
* detectar_pelicula_salva.py
* diferencia_2_img.py
* existe_diferencia_01.py
* funciones_imagenes.py

.. code:: Python

   import numpy as np
   import matplotlib.pyplot as plt
   import matplotlib.image as mpimg

   def Filtro_Media(img=0):
     ss = img.shape
     imgn = np.zeros(ss)
     for i in range(1,ss[0]-1):  
       for j in range(1,ss[1]-1):
         imgt = img[(i-1):(i+2), (j-1):(j+2)]
         res = np.mean(imgt)
         imgn[i,j] = res
     return imgn

   def Filtro_Mediana(img=0):
     ss = img.shape
     imgn = np.zeros(ss)
     for i in range(1,ss[0]-1):
       for j in range(1,ss[1]-1):
         imgt = img[(i-1):(i+2), (j-1):(j+2)]
      	 res = np.median(imgt)
      	 imgn[i,j] = res
     return imgn

   def corta_imagen(img=0, i1=0, i2=0, j1=0, j2=0):
     ''' hola como estas
      yo estoy bien
      --------------------------
     '''

     imgn = img[i1:i2, j1:j2]
     return imgn

   def ConvierteIMG_una(img=0):
     imgn = img[:, :, 0]
     return imgn   

   def despliega_img(imgB=''):
   #  plt.ion()
     plt.imshow(imgB, cmap='gray')
   #  plt.title(ns)
     #plt.axis("off")  # Oculta los ejes
   #  plt.ioff()
     plt.show()

   def LeeIMG_nombre(file=''):
       img = mpimg.imread(file)
       return img

   def LeeIMG(n=0):
     '''
     Lee una imagen donde:
  
     n - es el numero de archivos jpg que se va a leer
         n puede tomar valores de 1 a 27452
     |
     '''

     pat = '../../../temporal/'

     if n < 10:
       ns = '0000' + str(n)
     elif n < 100:
       ns = '000' + str(n)
     elif n < 1000:
       ns = '00' + str(n)
     elif n < 10000:
       ns = '0' + str(n)
     else:
       ns = str(n)

     file = pat + "output_" + ns + '.jpg'
   #  print(file)
     img = mpimg.imread(file)
     return img[:,:,0].copy()

   def LeeIMG_corta(n):
     img = LeeIMG(n)
     img = corta_imagen(img, 400, 700, 700, 1200)
     return img

   def Une_2_imagenes_Vertical(img1=0, img2=0):
     ss1 = img1.shape
     ss2 = img2.shape

     ny = ss1[1]
     if ny < ss2[1]:
       ny = ss2[1]
  
     nx = ss1[0]+ss2[0]

     img = np.zeros((nx, ny))

     img[1:(ss1[0]+1), :] = img1
     img[(ss1[0]+1+1):(ss2[0]+2), :] = img2

     return img


* lee_image_corta_salva.py
* lista_funciones.py
* pati.py

.. code:: Python

   def File():
     pat = '/Users/santosg/Hiram_datos'
     file = 'IMG_1952.MOV'
     return pat+'/'+file

* pelicula_cortada.mp4
* pelicula_recortada_recuadro.mp4
* pelicula_recortada_recuadro_procesada.mp4
* porcentaje_de_similitud.py
* porcentaje_de_similitud_2.py
* realiza_diferencia_img.py
* recorta_recuadro_video_ffmpeg.py
* subes.sh
* yolo_version.py
* yolov8n.pt


