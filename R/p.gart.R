p.gart <-
function(obs){
if(is.matrix(obs)==F){
print("The input must be a n by 3 matrix!")
}

else if(nrow(obs)==1){
n=obs[1,1]
m=obs[1,2]
x=obs[1,3]
p=1-(1-x/n)^(1/m)
Ip=m^2*n*(1-p)^(m-2)/(1-(1-p)^m)
Bp=1/(Ip^2)*m^2*(m-1)*n*(1-p)^(m-3)/(1-(1-p)^m)
pest=p-Bp
cat(paste("Equal group size: the estimated p by Gart's method is ", round(pest,digits=5),sep=""))
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
p=start

Ip=0
for(i in 1:d){
Ip=Ip+(obs[i,2]^2*obs[i,1]*(1-p)^(obs[i,2]-2))/(1-(1-p)^(obs[i,2]))
}

Bp=0
for(i in 1:d){
Bp=Bp+(obs[i,2]^2*(obs[i,2]-1)*obs[i,1]*(1-p)^(obs[i,2]-3))/(1-(1-p)^(obs[i,2]))
}
Bp=Bp/2/Ip^2
pest=p-Bp
cat(paste("Unequal group size: the estimated p by Gart's method is ", round(pest,digits=5),sep=""))
}

}
