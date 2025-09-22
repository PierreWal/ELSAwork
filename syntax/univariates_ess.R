### Univariates analysis

ess.s<-svydesign(~psu,weights=~anweight,strata=~stratum,data=ess,nest=T)

# Apply the function to all  variables 

rslt.u<-  lapply(c(uvars), function(v) vfreq(v, ess))

rslt.r <- lapply(c(rvars), function(v) wfreq2(v, ess.s) )



# Print the tables for each dependent variable using kable for better formatting
for (i in 1:length(rslt.u)) {
  cat('### ', labs[[c(uvars,dvars)[i]]], '\n')
  
  cat(paste0("#### **Unweighted frequency table of unrecoded *",c(uvars,dvars)[i],"* **", '\n'))
  print(rslt.u[[i]])
  cat('\n')
  
  
  cat(paste0("#### **Weighted frequency table of recoded *", c(rvars,dvars)[i],"* **", '\n'))
  print(rslt.r[[i]])
  
  cat('\n')
}

cat('\n:::')
cat('\n')


