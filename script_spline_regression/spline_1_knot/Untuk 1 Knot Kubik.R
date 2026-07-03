data<-read.csv("C:/Users/BiliL/Downloads/Project Coding GitHub/Praktikum_ART/applied-regression-analysis/data/Pertemuan 9 - Motorcycle Dataset.csv",header=TRUE)
data

plot(data$x,data$y)


#Maka digunakan regresi nonparametrik spline dengan fungsi truncated

#Spline Kubik Truncated 1 Titik Knot
GCV1=function(data)
{
  library(Matrix)
  library(pracma)
  para=0
  data=as.matrix(data)
  N=length(data[,1]) #Banyaknya Observasi
  M=2 #Banyaknya Kolom 
  m=ncol(data)-para-1  #m=banyaknya variabel nonparametrik
  dataA=data[,(para+2):M]
  dataA=as.matrix(dataA)
  F=diag(N)
  nk=22 #Banyaknya alternatif titik knot yg akan dicoba
  knot1=matrix(ncol=m,nrow=nk)
  for (i in (1:m)) #Membuat knot
  {
    a=seq(min(dataA[,i]),max(dataA[,i]),length.out=nk)
    knot1[,i]=t(as.matrix(a))
  }
  a1=length(knot1[,1])
  knot1=as.matrix(knot1[2:(a1-1),])
  aa=rep(1,N)
  data1=matrix(ncol=m,nrow=N)
  data2=data[,2:M] #Data yang variabel X saja
  nk1=nrow(knot1)
  GCV=as.matrix(rep(NA,nk1),ncol=1);colnames(GCV)<-"GCV"
  MSE=as.matrix(rep(NA,nk1),ncol=1);colnames(MSE)<-"MSE"
  SSE=rep(NA,nk1)
  SST=rep(NA,nk1)
  SSR=rep(NA,nk1)
  Koef.Determinasi=as.matrix(rep(NA,nk1),ncol=1);colnames(Koef.Determinasi)<-"Koef.Determinasi"
  knotke=matrix(c(1:nk1),ncol=1);colnames(knotke)<-"knot_ke"
  for (i in 1:nk1)
  {
    for (j in 1:m)
    {
      for (k in 1:N)
      {
        if(data[k,(j+para+1)]<knot1[i,j]) data1[k,j]=0 else data1[k,j]=data[k,(j+para+1)]-knot1[i,j]
      }
    }
    mx=as.matrix(cbind(aa,data2,(data2^2),(data2^3),(data1^3)))
    C=pinv(t(mx)%*%mx)
    B=C%*%(t(mx)%*%data[,1])
    ypred=mx%*%B
    residual=data[,1]-ypred
    SSE[i]=sum((residual)^2)
    SSR[i]=sum((ypred-mean(data[,1]))^2)
    SST[i]=sum((data[,1]-mean(data[,1]))^2)
    MSE[i]=SSE[i]/(N)
    Koef.Determinasi[i]=(SSR[i]/(SSR[i]+SSE[i]))
    A=mx%*%C%*%t(mx)
    A1=(F-A)
    A2=(sum(diag(A1))/N)^2
    GCV[i]=MSE[i]/A2
    
  }
  dataAll=as.matrix(cbind(GCV,Koef.Determinasi,MSE,knotke,knot1))
  dataG=dataAll[order(GCV),]
  
  cat("====================================================================================================================","\n")
  cat("HASIL GCV terkecil dengan 1 titik knot","\n")
  cat("====================================================================================================================","\n")
  print(((dataG[1,1:4])))
  cat("Dengan titik Knot","\n")
  print((knot1[dataG[1,4],]))
  cat("Nilai GCV 10 terkecil pertama","\n")
  print(dataG[1:10,])
  cat("\n")
  cat("====================================================================================================================","\n")
  mingcv=dataG[1,1]
  knotgcv=as.matrix(knot1[dataG[1,4],])
  knotgcv1=matrix(knotgcv,nrow=1)
  datagcv1=matrix(ncol=m,nrow=N)
  for (j in 1:m)
  {
    for (k in 1:N)
    {
      if (data[k,(j+para+1)]<knotgcv[j,1]) datagcv1[k,j]=0 else
        datagcv1[k,j]=data[k,(j+para+1)]-knotgcv[j,1]
    }
  }
  mxgcv=as.matrix(cbind(aa,data2,(data2^2),(data2^3),(datagcv1^3)))
  
  list(knotgcv=knotgcv1,mingcv=mingcv,mxgcv=mxgcv)
}

GCV1(data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Regresi Nonparametrik Spline Truncated 1 Titik Knot (Kubik)
#Estimasi Parameter


estimasi=function(data, mxgcv, alpha, type)
{
  data=as.matrix(data)
  n=nrow(data)
  mx=as.matrix(mxgcv)
  C=pinv(t(mx)%*%mx)
  B=C%*%(t(mx)%*%data[,1])
  cat("\n")
  cat("\n")
  cat("====================================================================================================================","\n")
  cat("hasil estimasi parameter","\n")
  cat("====================================================================================================================","\n")
  n1=nrow(B)
  yhat=mx%*%B
  res=data[,1]-yhat
  SSE=sum((res)^2)
  SSR=sum((yhat-mean(data[,1]))^2)
  SST=sum((data[,1]-mean(data[,1]))^2)
  MSE=SSE/(n)
  Koef.Determinasi=(SSR/(SSR+SSE))*100
  MSR=SSR/(n1-1)
  
  #Uji Simultan
  Fhit=MSR/MSE
  pvalue=pf(Fhit,(n1-1),(n-n1),lower.tail=FALSE)
  
  #Uji Parsial
  thit=rep(NA,n1)
  pvaluee=rep(NA,n1)
  MSE=as.numeric(MSE)
  SE=sqrt(diag(MSE*(pinv(t(mx)%*%mx))))
  for (i in 1:n1)
  {
    thit[i]=B[i,1]/SE[i]
    pvaluee[i]=2*(pt(abs(thit[i]),(n-n1),lower.tail=FALSE))
  }
  thit=as.matrix(thit)
  colnames(thit)<-"thitung"
  colnames(B)<-"parameter untuk Beta"
  
  tg1=cbind(B,thit,pvaluee)
  cat("====================================================================================================================","\n")
  cat("Estimasi Parameter","\n")
  cat("====================================================================================================================","\n")
  print(tg1)
  cat("\n")
  cat("Analysis of Variance","\n")
  cat("====================================================================================================================","\n")
  cat("Sumber  ","df","\t","SS","\t","\t","MS","\t","\t","Fhit","\n")
  cat("Regresi",(n1-1),"\t",SSR,"\t",MSR,"\t",Fhit,"\n")
  cat("Error  ",n-n1,"\t",SSE,"\t",MSE,"\n")
  cat("Total  ",n-n1,"\t",SST,"\n")
  cat("====================================================================================================================","\n")
  cat("s=",sqrt(MSE),"Rsq=", Koef.Determinasi,"\n")
  cat("pvalue(F)=", pvalue, "\n")
  yy=cbind(yhat, res)
  list(res=res, yhat=yhat, Koef.Determinasi=Koef.Determinasi)
}

model_truncated=estimasi(data, GCV1(data)$mxgcv, 0.05, 'gcv')  #Koefisien Determinasinya adalah sebesar 39.00%
prediksi <- model_truncated$yhat
plot(data$x, data$y)
lines(data$x, prediksi, col="red")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
