SimplexMethod<-function(mat){
  
  temp<-matrix(nrow=nrow(mat)-1,ncol=1)
  multiplierRow<-matrix(nrow=1, ncol=ncol(mat))
  
  for(i in 1:3){
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
mat<-matrix(c(7,11,1,0,0,0,0,77,10,8,0,1,0,0,0,80,1,0,0,0,1,0,0,9,0,1,0,0,0,1,0,6,-150,-175,0,0,0,0,1,0),ncol=8, nrow=5, byrow=TRUE)
pivotColumnIndex<-which.min(mat[nrow(mat),])
mat<-cbind(mat,c((mat[1,ncol(mat)]/mat[1,pivotColumnIndex]),(mat[2,ncol(mat)]/mat[2,pivotColumnIndex]),(mat[3,ncol(mat)]/mat[3,pivotColumnIndex]),(mat[4,ncol(mat)]/mat[4,pivotColumnIndex]), 0 ))
colnames(mat)<-c("x1","x2","s1","s2","s3","s4","Z","Solution", "TR")
rownames(mat)<-c("s1","s2","s3","s4","Z")
SimplexMethod(mat)
