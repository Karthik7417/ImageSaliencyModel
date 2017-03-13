cSalMapComb <- function(salMap, img, scalepref, orienpref, dwnScl, featBalan){
   if(!(exists('scalepref') & exists('var'))){
      scalepref = 2
   }
   if(!(exists('orienpref') & exists('var'))){
      orienpref = 1
   }
   if(!(exists('dwnScl') & exists('var'))){
      dwnScl = 1
   }
   if(!(exists('featBalan') & exists('var'))){
      featBalan = array(1,c(dim(salMap)[1]/dim(salMap[1])))
      }
   
   else if(length(featBalan) != dim(salMap)[1]){
      warning('Specified number of features does not match the number of features in the saliency map.')
   }
   else if(sum(featBalan) > 1){
      featBalan = featBalan/sum(featBalan)
   }
   
   ornSclArr = array(0,c(dim(salMap)[2]))
   
   if(length(scalepref) == 1){
      if(length(orienpref) == 1){
         ornSclArr = array(1, c(dim(salMap)[2]))
      }
      else{
         sclArr = array(1, c((dim(salMap)[2] - 2)/length(orienpref)))
         ornArr = orienpref
         ornSclArr[1] = sclArr[1]
         for(i in 2:(length(sclArr) - 1)){
            for(j in 1:length(ornArr)){
               ornSclArr[(i-2)*length(ornArr)+j] = sclArr[i]*ornArr[j]
            }
         }
         ornSclArr[length(ornSclArr)] = sclArr[length(sclArr)]
      }
      
   else{
        sclArr = scalepref
        if(length(orienpref) == 1){
           ornArr = array(1, c(dim(salMap)[2]-2)/(length(scalepref)-2))
           }

          else{
             ornSclArr = array(0, dim(salMap)[2])
             ornArr = orienpref
          }
        
        ornSclArr[length(ornSclArr)] = sclArr[length(sclArr)]
   }

[r,c,v] = dim(salMap)
   }
   subbands = 1:c
   smSclSalMap = array(0,c(r,c))
   list[mr, mc, clr] = dim(img)
   nrmSalMap = salMap
   
   list[minr, minc] = dim(salMap[1,1])
   x = 1:minc
   y = 1:minr
   
   
   for(j in 1:r){
      for(i in 1:c){
         smSclSalMap[j,i] = array(0,c(mr,mc))
         list[tr, tc] = length(salMap[j,i])
         
         if(dwnScl == 1){
            numLevs = log2(max(tr/minr, tc/minc))
            sclSalMap = blurDn(salMap[j,i], numLevs,'binom5')
         }
         else{
            numLevs = round(log2(max(mr/tr, mc/tc)))
            upSclSalMap = upBlr(salMap[j,i], numLevs,'binom5')
            
            list[tyr, tyc] <- dim(upSclSalMap)
            rwOfset = floor((try-mr)/2)
            colOfset = floor((tyc-mc)/2)
            
            sclSalMap = upSclSalMap[seq(floor((tyr-mr)/2+1),mr + floor((try-mr)/2)), seq(floor((trc-mc)/2 )+1,mc+floor((tyc-mc)/2))]
         }
         
         nrmSalMap[j,i] = sclSalMap
         nrmSalMap[nrmSalMap[j,i] < 0] = 0
         #nrmSalMap[j,i] = mat2gray(nrmSalMap[j,i])
         
         BW = imregional(nrmSalMap[j,i] , 8)
         nGM = nrmSalMap[nrmSalMap[j,i] != 1]
         BW = BW*nGM
         locAvg = BW*nrmSalMap[j,i]
         
         if(sum(BW) == 0){
            locAvg = 0
         } 
         else{
            locAvg = sum(locAvg)/ sum(BW)
         }
         
         smSclSalMap[j,i] = (nrmSalMap[j,i] *100 * (100 - locAvg*100)^2)/(1+log(sum(BW)))
      }
   }
   
   avgMap = array(0, c(dim(smSclSalMap[1,1])))
   
   for(i in 2:c){
      for(j in 1:r){
         avgMap = avgMap + featBalan[j]*ornSclArr[i]*smSclSalMap[j,i]/(r*(c-1))
      }
   }
   #salImg = mat2gray(avgMap)
}
