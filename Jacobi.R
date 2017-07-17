n<-3
iter<-10
A<-matrix(c(3,-0.1,-0.2,7.85,0.1,7,-0.3,-19.3,0.3,-0.2,10,71.4),nrow=n,ncol=n+1,byrow=T)
#A<-matrix(c(3,2,1,1,5,3,4,2,1,1,-1,1),nrow=n,ncol=n+1,byrow=T)
x<-rep(0, n)
v<-rep(0, n)
print(A)
conver<-1
for (i in 1:n){
	suma=0
	for(j in 1:n){
		if(i!=j){
			suma=suma+A[i,j]		
		}	
	}
	if(A[i,i]<suma){
		conver<-0
		break	
	}
}

if(conver==1){
	for (k in 1:iter){
		for (i in 1:n){
			Rx<-0;
			for(j in 1:n){
				if(j!=i){

					Rx<-Rx+A[i,j]*x[j]			
				}
			}

			v[i]<-(A[i,n+1]-Rx)/A[i,i]
			}
		x<-v
	}

print(x)
}else{
	print('El sistema no converge a una solucion')
}
