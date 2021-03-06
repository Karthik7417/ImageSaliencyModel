require('magick','imager','ptw')
library(ptw)
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

hMapClrOppGGDNM <- function(img, filterType, levelNum, krnSz){
   if(!(exists('krnSz') & exists('var'))){
      krnSz = 7  #kernel size
   }
   if(!exists('filterType')){
      filterType = 'sp5Filters' #number of spatial orientations being extracted
   }
   
   filtLev = as.double(substr(filterType, 3,3)) 
   
   if(!exists('levelNum')){
      levelNum = 4 #number of spatial frequencies being analyzed
   }
   
   sigma_nsq = 0.4
   
   beta = seq(0.02, 2, 1e-6)
   jb = (0.5 - 1/ beta)
   jaa = beta/2
   jaba = (gamma(0.5)^3 * gamma(1.5))
   jabb = gamma(1/beta) ^3*gamma(1.5)
   J3 = log(jaa * sqrt(jaba/jabb)) + jb
   
   if(spectrum(img) > 1){
      
      rg = R(img) - G(img)
      gr = G(img) - R(img)
      by = B(img) - 0.5*(R(img) + G(img))
      yb = -by
      rg[rg<0] = 0
      gr[gr<0] = 0
      by[by<0] = 0
      yb[yb<0] = 0
      
      bwimg = 0.2125 * R(img) + 0.7154 * G(img) + 0.0721 * B(img) #intensity calculation 
      
      img.df <- array(0, c(width(img),height(img), 5))
      
      img.df[,,1] = rg*255
      img.df[,,2] = gr*255
      img.df[,,3] = by*255
      img.df[,,4] = yb*255
      img.df[,,5] = bwimg*255
   }
   
   list[pyr, pind] <- buildSpyr(img.df[,,1], levelNum, filterType, 'reflect1')
   
   imorg = ind2wtree(abs(pyr), pind)
   subbands = 1:length(imorg)
   
   hEst = 1:length(subbands)
   sizeMinrc = dim(imorg[1])/krnSz
   minr = sizeMinrc[1]
   minc = sizeMinrc[2]
   
   salmap = 1:length(subbands)
   
   for(ii in 1:length(subbands)){
      sizerc = floor(dim(imorg[ii]/krnSz))
      r = sizerc[1]
      c = sizerc[2]
      NegEst = array(0,c(r*c,2))
      offset = dim(imorg[ii]) - c(r*krnSz, c*krnSz)
      lOffset = floor(offset/2)
      rOffset = offset - lOffset
      tmp = array()
      for(jj in 1:krnSz){
         for(kk in 1:krnSz){
            tmp = do.call('rbind', list(tmp, imorg[ii,seq(jj+lOffset[1],krnSz,length(imorg)-rOffset[1]), seq(kk+lOffset[2],krnSz:length(imorg)-rOffset[2])]))
         }
      }
      
      list[NegEst[,1], NegEst[,2]] <- negnMth(tmp, J3, beta,1e-6,1)
      NegEst = array(NegEst, c(r,c,2))
      gBeta <- gamma(1/NegEst[,,2])
      
      hEst = 1/NegEst[,,2] - log(NegEst[,,2])/(2*NegEst[,,1]*gBeta)
      hEst[ii, is.nan(hEst[ii])] = 0
      hEst[ii, is.infinite(hEst[ii])] = 0
      
      orn = filtLev + 1 
      salmap[ii] = array(0, c(krnSz,r,c))
      
      for(jj in 1:r){
         for(kk in 1:c){
            salmap[ii, seq(krnSz*jj-(krnSz-1),krnSz*jj),seq(krnSz*kk-(krnSz-1),krnSz*kk)] = hEst[ii, jj, kk]
         }
      }
      
      salmap[clr,ii] = padzeros(salmap[ii], floor(length(imorg) - length(salmap[ii]))/2, side = 'left')
      salmap[clr,ii] = padzeros(salmap[ii], ceiling(length(imorg) - length(salmap[ii]))/2, side = 'right')
 }
   
} 



