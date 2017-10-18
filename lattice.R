dim_N <- 10

sum_nn <- function(mat,i,j){ 
	if(!is.matrix(mat)){
		stop("Argument must be a matrix")
	} else if (i==1 && j==1){
		return(mat[i+1,j]+mat[i,j+1])
	} else if (i==dim_N && j==dim_N){
		return(mat[i-1,j]+mat[i,j-1])
	} else if (i==dim_N && j==1){
		return(mat[i-1,j]+mat[i,j+1])
	} else if (i==1 && j==dim_N){
		return(mat[i+1,j]+mat[i,j-1])
	} else if (i==1){
		return(mat[i+1,j]+mat[i,j+1]+mat[i,j-1])
	} else if (i==dim_N){
		return(mat[i-1,j]+mat[i,j+1]+mat[i,j-1])
	} else if (j==1){
		return(mat[i+1,j]+mat[i-1,j]+mat[i,j+1])
	} else if (j==dim_N){
		return(mat[i+1,j]+mat[i-1,j]+mat[i,j-1])
	} else {
		return(mat[i+1,j]+mat[i-1,j]+mat[i,j-1]+mat[i,j+1])
	}
}

lattice <- array(rep(0,dim_N^2), dim=c(dim_N,dim_N))
for(i in 1:dim_N){
	for(j in 1:dim_N){
		if(runif(n=1,min=-1,max=1)>=0){
			lattice[i,j]<- 1
		} else{
			lattice[i,j]<- -1
		}
	}
}