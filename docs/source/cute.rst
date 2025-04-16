cute
====

**/Users/santosg/Cute_Programas/CUTE_feb1924_NUEVA_CHAMBA/CUTE_Prog_ART_jul1023_chamba_feb1824**

prog_prueba.R

.. code:: R

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


SolucionXPU_3_salto_chamba.R

.. source:: R

   SolucionXPU_3_salto_chamba <- function(del=0, pp=0){
      ss = dim(pp)

      n = ss[1]
   
      xx = matrix(rep(0,3*n), ncol=3)

      dwx1 = DWX1 *sqrt(del)*runif(n)
      dwx2 = DWX2 *sqrt(del)*runif(n)
      dwx3 = DWX3 *sqrt(del)*runif(n)
   
      x1 = c(.7, .7,.5)
      xx[1,] = x1
      p1 = pp[1,]
      w1 = dwx1[1]
      w2 = dwx2[1]
      w3 = dwx3[1]

      u = Calu1u2u3(x1, p1,1)

      for (i in 2:n){
         r = funX_ART2_chamba (x1, u)
         z = c(0,0,0)
         if (length(which(i == II)) > 0){
            z = K(rnorm(3))
         }

         x21 = x1[1] + (TT[i]-TT[i-1]) * r[1] + w1+ x1[1]*u[1]*z[1]
         x22 = x1[2] + (TT[i]-TT[i-1]) * r[2] + w2+ x1[2]*u[2]*z[2]
         x23 = x1[3] + (TT[i]-TT[i-1]) * r[3] + w3+ x1[3]*u[3]*z[3]
         x1 = c(x21, x22, x23)
         xx[i,] = x1
         p1 = pp[i,]
         w1 = dwx1[i]
         w2 = dwx2[i]
         w3 = dwx3[i]
         u = Calu1u2u3(x1, p1, i )
      }

      uu = matrix(rep(0,3*n), ncol=3)

      for (i in 1:n){
         pi = pp[i,]
         xi = xx[i,]
         uu[i, ] = Calu1u2u3(xi,pi, i)
      }
      res = list(xx=xx, uu=uu)
   }


SolucionPPU_3_salto_chamba.R

.. code:: R

   SolucionPPU_3_salto_chamba <- function(del=0, xx=0, uu=0){ 
	ss = dim(xx)
	n = ss[1]
	
	dwp = DWP*sqrt(del)*runif(n)
	
	pp = matrix(rep(0,3*n), ncol=3)
	
	pp[n,] = c(0,0,0)
	
	p2 = pp[n,]
	u2 = uu[n,]
	w2 = dwp[n]
	x2 = xx[n,]	
	
	for (i in (n-1):1){
		r = funP_ART2_chamba(x2, p2,u2)
		z = c(0,0,0)
		if (length(which(i == II)) > 0){
			z = K(rnorm(3))
		}
		p11 = p2[1] - (TT[i+1]-TT[i]) * r[1] - p2[1] * w2 + u2[1] * x2[1]*z[1]
		p12 = p2[2] - (TT[i+1]-TT[i]) * r[2] - p2[2] * w2 + u2[2] * x2[2]*z[2]
		p13 = p2[3] - (TT[i+1]-TT[i]) * r[3] - p2[3] * w2 + u2[3] * x2[3]*z[3]
		p2 = c(p11, p12, p13)
		pp[i,] = p2
		u2 = uu[i,]
		w2 = dwp[i]
		x2 = xx[i,]
	}
	res = pp
   }





