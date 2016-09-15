p.mle <-
function(obs){
if(is.matrix(obs)==F){
print("The input must be a n by 3 matrix!")
}

else if(nrow(obs)==1){
n=obs[1,1]
m=obs[1,2]
x=obs[1,3]
pest=1-(1-x/n)^(1/m)
cat(paste("Equal group size: the estimated p by maximum likelihood method is ", round(pest,digits=5),sep=""))
}

else if(nrow(obs)>1){
d=nrow(obs)

Sp=function(p){
score=0
for(i in 1:d){
score=score+obs[i,2]*obs[i,3]/(1-(1-p)^obs[i,2])-obs[i,1]*obs[i,2]
}
return(score)
}

start=0
end=1
L=end-start
step=0
while(L>10^-5){
middle=(start+end)/2
if(Sp(middle)*Sp(start)<0){
end=middle
}
else{
start=middle
}
L=end-start
step=step+1
}
pest=start
cat(paste("Unequal group size: the estimated p by maximum likelihood method is ", round(pest,digits=5),sep=""))
}
}
