SimplexMethod<-function(mat){
  
  temp<-matrix(nrow=nrow(mat)-1,ncol=1)
  multiplierRow<-matrix(nrow=1, ncol=ncol(mat))
  flag<-0
  while(flag!=1){
    flag<-1
    for(i in 1:(ncol(mat)-1)){
      if(mat[nrow(mat),i]<0){
        flag<-0
      }
    }
    
    if(flag==1){
      return(print("Ok"))
    }
    for(i in 1:nrow(temp)){
      temp[i,]<-mat[i,ncol(mat)]
      if(temp[i,]<0){
        temp[i,]<-Inf
      }
    }#put TR on temp 
    
    pivotColumnIndex<-which.min(mat[nrow(mat),])#search the index of the minimum on last row
    solutionIndex<-which.min(temp)#search for the index of minimum on TR
    pivotElement<-mat[solutionIndex, pivotColumnIndex]#Get the pivot element
    
    for(i in 1:ncol(mat)){
      multiplierRow[,i]<-mat[solutionIndex, i]
    }#Get the multipler row
    multiplierRow<- multiplierRow/pivotElement#Normalize
    mat[solutionIndex,]<-multiplierRow
    
    for(i in 1:nrow(mat)){
      if(i!=solutionIndex){
        mat[i,]<-mat[i,]-(multiplierRow*mat[i,pivotColumnIndex])
      }
    }    
    
    #Update TR here
    pivotColumnIndex<-which.min(mat[nrow(mat),])
    for(i in 1:nrow(mat)-1){
      temp[i,]<-  mat[i, ncol(mat)-1]/mat[i,pivotColumnIndex]
      mat[i,ncol(mat)]<-temp[i,1]
    }
    mat[nrow(mat),ncol(mat)]<-0
    print(mat)
  }
  return(print("Ok"))
}

mat<-matrix(c(0.44, 0, 0, 0, 0, 0, 121.46,
              0, 0.88, 0, 0, 0, 0, 105.4,
              0, 0, 0.22, 0, 0, 0, 2732.08,
              0, 0, 0, 0.44, 0, 0, 2620.16,
              0, 0, 0, 0, 0.33, 0, 4965.54,
              0, 0, 0, 0, 0, 4.95, 76.89,
              0.06, 0.35, 1.47, 0.47, 0.23, 0.02, 0
),nrow=7,ncol=7,byrow=TRUE)
mat<-t(mat)
temp<-matrix(nrow=nrow(mat),ncol=(ncol(mat)-1))
noOfSlacks<-6
temp2<-matrix(nrow=noOfSlacks, ncol=noOfSlacks)#slack matrix
temp3<-matrix(nrow=1, ncol=noOfSlacks)#slack row
temp5<-matrix(nrow=nrow(mat), ncol=1)

for(i in 1:nrow(mat)){
  temp5[i,]<-mat[i,ncol(mat)]
}
for(i in 1:nrow(temp)){
  for(j in 1:ncol(temp)){
    temp[i,j]<-mat[i,j]
  }
}#exclude solution column

for(i in 1:noOfSlacks){
  for(j in 1:noOfSlacks){
    if(i==j){
      temp2[i,j]<-1
    }else{
      temp2[i,j]<-0  
    }
    
  }
}#generate slack matrix

for(i in 1:noOfSlacks){
  temp3[1,i]<-0  
}

temp2<-rbind(temp2,temp3)#append the slack matrix
temp<-cbind(temp,temp2)

temp4<-matrix(nrow=nrow(temp), ncol=1)

for(i in 1:nrow(temp4)){
  if(i==nrow(temp4)){
    temp4[i,]<-1
  }else{
    temp4[i,]<-0 
  }
}
temp<-cbind(temp,temp4)
temp<-cbind(temp,temp5)

for(i in 1:(ncol(temp)-1)){
  
  if(i!=ncol(temp)-1){
    temp[nrow(temp),i]<- -temp[nrow(temp),i]
  }
}
pivotColumnIndex<-which.min(temp[nrow(temp),])

TR<-matrix(ncol=1,nrow=nrow(temp))
for(i in 1:nrow(TR)-1){
  TR[i,1]<- temp[i,ncol(temp)]/ temp[i,pivotColumnIndex]
}
TR[nrow(temp),1]<-0
temp<-cbind(temp, TR)
pivotColumnIndex<-which.min(temp[nrow(temp),])
SimplexMethod(temp)