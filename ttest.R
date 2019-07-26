rm(list=ls())
t_test<- function(f){
  a=read.table(f,header=T,sep="\t")  
  #预生成2个长度与输入文件行数相同的全为0的向量，将用于存储p value和差异倍数（log2FC）
  library(car)
  library(carData)
  Pvalue<-c(rep(0,nrow(a)))   
  log2_FC<-c(rep(0,nrow(a)))
  group=as.factor(c(1,1,1, 2,2,2))
  # 2~4列是处理组1,5~7列是处理组2；
  #将使用循环对每一行进行t检验
  #如果某一行两组的标准差都等于0，将无法进行t检验，所以这些行使用NA替代
  #每一行计算得到p value和log2FC将被加入原文件的后两列；
  #计算log2FC时，每组均值加0.001，是为了防止分母为0导致bug；
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
  # 对p value进行FDR校正
  fdr=p.adjust(Pvalue, "BH")
  # 在原文件后面加入log2FC，p value和FDR,共3列；
  out<-cbind(a,log2_FC,Pvalue,fdr)
  name<-paste0(f,"_t")
  write.table(out,file=name,quote=FALSE,sep="\t",row.names=FALSE)
}

files<- dir(pattern = "DS*")
for (f in files){
  t_test(f)
}