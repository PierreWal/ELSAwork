### Univariates analysis

elsaw$AgeGap31.fr<-elsaw$AgeGap31.f
elsaw$AgeGap32.fr<-elsaw$AgeGap32.f

elsaw.s<-svydesign(~1,weights=~w10xwgt,data=elsaw)

# Apply the function to all  variables 

rslt.u<-  lapply(c(uvars[uvars!="exlo80.f" & uvars!="exlo90.f"& uvars!="psagf.f"],dvars), function(v) vfreq(v, elsaw))

rslt.r <- lapply(c(rvars[rvars!="exlo80.fr" & rvars!="exlo90.fr"& rvars!="psagf.fr"],dvars), function(v) wfreq2(v, elsaw.s) )



# Print the tables for each dependent variable using kable for better formatting
for (i in 1:length(rslt.u)) {
  cat('### ', labs[[c(uvars[uvars!="exlo80.f" & uvars!="exlo90.f"& uvars!="psagf.f"],dvars)[i]]], '\n')
  
  cat(paste0("#### **Unweighted frequency table of unrecoded *",c(uvars[uvars!="exlo80.f" & uvars!="exlo90.f"& uvars!="psagf.f"],dvars)[i],"* **", '\n'))
  print(rslt.u[[i]])
  cat('\n')
  
  
  cat(paste0("#### **Weighted frequency table of recoded *", c(rvars[rvars!="exlo80.fr" & rvars!="exlo90.fr"& rvars!="psagf.fr"],dvars)[i],"* **", '\n'))
  print(rslt.r[[i]])
  
  cat('\n')
  
}

cat('\n')
cat('\n')
cat('### ', labs[["psagf"]], '\n')
cat('\n')

cat('\n')
cat('**Unweighted results**') 
cat("\n")

psagf<-ifelse(elsaw$psagf>=0,elsaw$psagf,NA)
kable(round(
  summarytools::descr(
    psagf, 
    stats = c("common", "n"), 
    transpose=T),1
)
)

cat('\n')
cat('**Weighted results**') 
cat("\n")

w10xwgt<-ifelse(elsaw$psagf>=0,elsaw$w10xwgt,0)
kable(round(
  summarytools::descr(psagf, 
                      stats = c("common", "n"), 
                      transpose=T,
                      weights = w10xwgt
  ),1
)
)


par(mfrow = c(1, 2))

hist(elsaw$psagf,
     main=labs[["psagf"]],xlab="Age")
cat('\n')

wtd.hist(psagf,
         weight=w10xwgt,
         main=paste0(labs[["psagf"]], " (weighted)"),xlab="Age")
cat('\n')

cat('\n')
cat('\n')
cat('### ', labs[["exlo80"]], '\n')
cat('\n')

cat('\n')
cat('**Unweighted results**') 
cat("\n")

exlo80<-ifelse(elsaw$exlo80>=0,elsaw$exlo80,NA)
kable(round(
  summarytools::descr(
    exlo80, 
    stats = c("common", "n"), 
    transpose=T),1
)
)

cat('\n')
cat('**Weighted results**') 
cat("\n")

w10xwgt<-ifelse(elsaw$exlo80>=0,elsaw$w10xwgt,0)
kable(round(
  summarytools::descr(exlo80, 
                      stats = c("common", "n"), 
                      transpose=T,
                      weights = w10xwgt
  ),1
)
)


par(mfrow = c(1, 2))

hist(elsaw$exlo80,          
     main="Exp (%) that will live to [age]",xlab="Expected Age"
)
cat('\n')

wtd.hist(exlo80,
         weight=w10xwgt,
         main="Idem  (recoded & weighted)",xlab="Expected Age")
cat('\n')

cat('\n')
cat('\n')
cat('### ', labs[["exlo90"]], '\n')
cat('\n')

cat('\n')
cat('**Unweighted results**') 
cat("\n")

exlo90<-ifelse(elsaw$exlo90>=0,elsaw$exlo90,NA)

kable(round(
  summarytools::descr(
    exlo90, 
    stats = c("common", "n"), 
    transpose=T),1
)
)


cat('\n')
cat('**Weighted results**') 
cat("\n")

w10xwgt<-ifelse(elsaw$exlo90>=0,elsaw$w10xwgt,0)

kable(round(
  summarytools::descr(exlo90, 
                      stats = c("common", "n"), 
                      transpose=T,
                      weights = w10xwgt
  ),1
)
)



cat('\n')

par(mfrow = c(1, 2))

hist(elsaw$exlo90,
     main="Exp. prob. of living beyond 85",xlab="Probability")
cat('\n')

wtd.hist(exlo90,
         weight=w10xwgt,
         main="Idem  (recoded & weighted)",xlab="Probability")
cat('\n')

cat('\n')
cat('\n')
cat('### ', labs[["AgeGap.r"]], '\n')
cat('\n')

cat('\n')
cat('**Unweighted results**') 
cat("\n")

AgeGap.r<-ifelse(elsaw$AgeGap.r>=0,elsaw$AgeGap.r,NA)

kable(round(
  summarytools::descr(
    AgeGap.r, 
    stats = c("common", "n"), 
    transpose=T),1
)
)


cat('\n')
cat('**Weighted results**') 
cat("\n")

w10xwgt<-ifelse(elsaw$AgeGap.r>=0,elsaw$w10xwgt,0)

kable(round(
  summarytools::descr(AgeGap.r, 
                      stats = c("common", "n"), 
                      transpose=T,
                      weights = w10xwgt
  ),1
)
)



cat('\n')

par(mfrow = c(1, 2))

hist(elsaw$AgeGap.r,
     main="Gap between actual and perceived age",xlab="Years")
cat('\n')

wtd.hist(AgeGap.r,
         weight=w10xwgt,
         main="Idem  (weighted)",xlab="Years")
cat('\n')


cat('\n:::')
cat('\n')