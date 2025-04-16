cute
====

**/Users/santosg/Cute_Programas/CUTE_feb1924_NUEVA_CHAMBA/CUTE_Prog_ART_jul1023_chamba_feb1824**

prog_prueba.R

.. code:: Bash

   rm(list=ls())

   #source('CreaFolder.R')
   source('SolucionXPU_3_salto_chamba.R')
   source('SolucionPPU_3_salto_chamba.R')
   source('funciones_modelos_A.R')
   source('SalvaGrafica.R')
   source('funciones_M.R')
   source('CreaFolder.R')
   
   beta1 =.2
   delta1 = .3
   epsilon1 = .3

   del = .001
   x2 = 60

   npun = GenNP(0, x2, del)
   cat(' npun: ', npun,'\n')

   tiempo = seq(0, x2, length.out = npun)
   del = tiempo[2]-tiempo[1]

   cat(' del: ', del,'\n')

   parpois = 0.4
   #TETA = 0.001
   #TETA = 0.01
   # TETA = 0.1 #bien
   TETA = .2

   ss = funcion_poisson(60, parpois)
   tt = as.vector(ss[[1]])

   res = EncIndices(tiempo, tt)
   II = as.vector(res[[2]])
   print(II)

   TT = as.vector(res[[1]])
   npun = length(TT)
   cat(' npun: ', npun,'\n')

   cc = rep(0, npun)
   pp =  matrix(rep(c(1, 0, 0), c(npun, npun, npun)), ncol=3)

   param = matrix(c(.17, .17, .17, 12, .15, .2, .25, 26, .01,.01,.1,12, .04,.01,.15,24, .07,.03,.05,18, 0, 0, 0, 
   25), 
   ncol=6)

   m = 1
   DWX1 = param[1,m]
   DWX2 = param[2,m]
   DWX3 = param[2,m]
   DWP =  param[3,m]
   kk =   param[4,m]


   A1 <<- .4
   B1 <<- .42
   C1 <<- .19

   fig        = 'fig_'
   prefijo = paste('fac_','TETA', TETA,'_A1', A1, '_B1', B1, '_C1', C1, '_dwx1_',DWX1,'_dwx2_', 
   DWX2,'_dwp_',DWP,':b', 
   beta1,'d', delta1,'e',epsilon1, sep='')
   pat      = getwd()

   CreaFolder(prefijo)

   for (jj in 1:10){
	cat('iter= ', jj, '\n')
	
	R <- SolucionXPU_3_salto_chamba(del, pp)
	
	if (jj == jj){
        SalvaGraficaJPG(TT, R$xx[,1], pat, fig, prefijo, jj,kk,'t', 'X1')
        SalvaGraficaJPG(TT, R$xx[,2], pat, fig, prefijo, jj,kk,'t', 'X2')
		SalvaGraficaJPG(TT, R$xx[,3], pat, fig, prefijo, jj,kk,'t', 'X3')
		
		library("scatterplot3d")
		
		fig_namet3D = paste(fig, prefijo, '_', jj,'_3D_',kk, '.jpg', sep='')
		jpeg(file = file.path(pat, prefijo,fig_namet3D))
		par(mai=c(1,1,.5,.5))
		scatterplot3d(R$xx[,1], R$xx[,2], R$xx[,3], type='l', box=F, lwd=3, color='blue', xlab='x1', 
   ylab='x2', zlab='x3')
		dev.off()

		SalvaGraficaJPG(R$xx[,1], R$xx[,2], pat, fig, prefijo, jj,kk,'x1', 'x2')
		SalvaGraficaJPG(R$xx[,2], R$xx[,3], pat, fig, prefijo, jj,kk,'x2', 'x3')
		SalvaGraficaJPG(R$xx[,1], R$xx[,3], pat, fig, prefijo, jj,kk,'x1', 'x3')
		
	    SalvaGraficaJPG(TT, R$uu[,1], pat, fig, prefijo, jj,kk,'t', 'u1')
	    SalvaGraficaJPG(TT, R$uu[,2], pat, fig, prefijo, jj,kk,'t', 'u2')
	    SalvaGraficaJPG(TT, R$uu[,3], pat, fig, prefijo, jj,kk,'t', 'u3')	    
	}
	
	pp <- SolucionPPU_3_salto_chamba(del, R$xx, R$uu)	

	if (jj == jj){
		SalvaGraficaJPG(TT, pp[,1], pat, fig, prefijo, jj,kk,'t', 'p1')
		SalvaGraficaJPG(TT, pp[,2], pat, fig, prefijo, jj,kk,'t', 'p2')
		SalvaGraficaJPG(TT, pp[,3], pat, fig, prefijo, jj,kk,'t', 'p3')
	}	
   }





