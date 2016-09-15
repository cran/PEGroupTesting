p.order <-
function(obs){
if(is.matrix(obs)==F){
print("The input must be a 1 by 3 matrix!")
}

else if(nrow(obs)>1){
print("The order statistics method is only for equal group size. Please use Gart's method for unequal group size.")
}

else if(nrow(obs)==1){
n=obs[1,1]
k=obs[1,2]

pws=function(w,s){
if(s==0){
return(1/(k*n+1))
}
else if(s==n){
return(1-1/k*beta(1/k,n+1))
}
else{
return(1-(k*n-k*s+w)/(k*n-k*s+1)*beta(s,n-s+1/k+1)/beta(s,n-s+1))
}
}

p1=pws(1,obs[1,3])
Bpp=function(ww){
Bias_part1=0
for(i in 0:n){
Bias_part1=Bias_part1+pws(ww,i)*dbinom(i,n,1-(1-p1)^k)
}
Bias=abs(Bias_part1-p1)
return(Bias)
}

cand=seq(0,1,0.002)
Bias_cand=sapply(cand,Bpp)
w1=cand[which.min(Bias_cand)]
p2=pws(w1,obs[1,3])

Bpp1=function(www){
Bias_part2=0
for(i in 0:n){
Bias_part2=Bias_part2+pws(www,i)*dbinom(i,n,1-(1-p2)^k)
}
Bias2=abs(Bias_part2-p2)
return(Bias2)
}

Bias_cand2=sapply(cand,Bpp1)
w2=cand[which.min(Bias_cand2)]

cat(paste("Equal group size: the estimated p by order statistics method is ", round(pws(w2,obs[1,3]),digits=5),sep=""))
}
}
