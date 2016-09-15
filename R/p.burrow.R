p.burrow <-
function(obs){
if(is.matrix(obs)==F){
print("The input must be a 1 by 3 matrix!")
}

else if(nrow(obs)>1){
print("Burrow's method is only for equal group size. Please use Gart's method for unequal group size.")
}

else if(nrow(obs)==1){
n=obs[1,1]
m=obs[1,2]
x=obs[1,3]
pest=1-((2*m*n-2*m*x+m-1)/(2*m*n+m-1))^(1/m)
cat(paste("Equal group size: the estimated p by Burrow's method is ", round(pest,digits=5),sep=""))
}
}
