rm(list=ls())
t_test<- function(f){
  a=read.table(f,header=T,sep="\t")  
  
  library(car)
  library(carData)
  Pvalue<-c(rep(0,nrow(a)))   
  log2_FC<-c(rep(0,nrow(a)))
  group=as.factor(c(1,1,1, 2,2,2))
  
  for(i in 1:nrow(a)){
    if(sd(a[i,2:4])==0&&sd(a[i,5:7])==0){
      Pvalue[i] <- "NA"
      log2_FC[i]<- "NA" 
    }
    else if(setequal(a[i,2:4],a[i,5:7])){
      Pvalue[i] <- "NA"
      log2_FC[i]<- "NA"
    }
    else if((leveneTest(y=unlist(c(a[i,2:7])),group=group,center="mean")$Pr[1])>0.05){
      y=t.test(as.numeric(a[i,2:4]),as.numeric(a[i,5:7]),var.equal=TRUE)
      Pvalue[i]<-y$p.value
      log2_FC[i]<-log2((mean(as.numeric(a[i,2:4]))+0.001)/(mean(as.numeric(a[i,5:7]))+0.001))
    }
    else{
      y=t.test(as.numeric(a[i,2:4]),as.numeric(a[i,5:7]))
      Pvalue[i]<-y$p.value
      log2_FC[i]<-log2((mean(as.numeric(a[i,2:4]))+0.001)/(mean(as.numeric(a[i,5:7]))+0.001))
    }
  }
 
  fdr=p.adjust(Pvalue, "BH")
  
  out<-cbind(a,log2_FC,Pvalue,fdr)
  name<-paste0(f,"_t")
  write.table(out,file=name,quote=FALSE,sep="\t",row.names=FALSE)
}

files<- dir(pattern = "DS*")
for (f in files){
  t_test(f)
}
