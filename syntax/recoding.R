############################################################################################
## Renaming voting variables
#elsaw$VotGE19<-elsaw$scsptrvt


labs<-list(
  AgeGap.r="Gap: actual and perceived age (cont.)",
  AgeGap31.f="Gap: actual and perceived age (exact)",
  AgeGap32.f="Gap: actual and perceived age (3 years)",
  cfmap.f="Self-rated mental abilities V1",
  cfmap2.f="Self-rated mental abilities V2",
  cfmap3.f="Self-rated mental abilities V3",
  cfmetm.f="Self-rated memory V1",
  cfmetm2.f="Self-rated memory V2",
  cfmetm3.f="Self-rated memory V3",
  digran.f="Whether has any grandchildren",
  dimarr.f="Marital/relationship status",
  edqual.f="Educational level",
  erfvol.f="Volunteered",
  erlvolba.f="Cared for baby/child",
  erlvolpe.f="Whether cared for a sick/frail person",
  ethnic.f="White vs Non-white",
  exrela.f="Too little money to spend on their needs?",
  exlo80="Expectation (%) that will live to [age]",
  exlo90="Expectation (%) that will live to be 85 years old or more",
  exlo90d.f="Whether reckon that  will live to be 85 yo or more (dichotomic)",
  headlba.f="Difficulty bathing or showering",
  headlbe.f="Difficulty getting in and out of bed",
  hehelf.f="Self-reported general health V1",
  hehelf2.f="Self-reported general health V2",
  hehelf3.f="Self-reported general health V3",
  hhdtypb.f="Recoded household type, 4 categories",
  hobb.f="Internet connection?",
  iafcon.f="Financial situation",
  iafind.f="Who has the final say in big financial decisions",
  indsex.f="Sex",
  psagf="Self-perceived age",
  pscedd.f="Whether happy much of the time last  week",
  pscede.f="Feels lonely?",
  sclfcoa.f="I want to give back to my community",
  scorgch.f="Whether member of a charity",
  scorgpo.f="Whether member of a party, union or environmental groups",
  scorgrl.f="Whether member of a church or other religious group",
  spcar.f="Whether has use of car/van, as a driver or a passenger",
  sptraa.f="How often respondent uses public transport",
  tenureb.f="Household Tenure",
  VotGE01.f="Whether voter at the 2001 GE",
  VotGE05.f="Whether voter at the 2005 GE",
  VotGE15.f="Whether voter at the 2015 GE",
  VotGE17.f="Whether voter at the 2017 GE",
  VotGE19.f="Whether voter at the 2019 GE",
  w10nssec5.f="NS-SEC 5 category",
  wpdes.f="Economic activity",
  wpdes.fr="Economic activity (recoded)",
  AgeCat2.fr="Recoded age, 2 category",
  AgeCat3.fr="Recoded age, 3 category",
  VotRec2.fr="Past voting record, dichotomic",
  VotRec0.fr="Past voting record, v1",
  VotRec01.fr="Past voting record, v2"
)



## Recode

elsaw<-elsaw|>mutate(
  AgeCat2.fr=as.factor(case_when(
    indager>=50 & indager<70 ~ "50-69",
    indager>=70 ~ "70+"
  )),
  AgeCat3.fr=as.factor(case_when(
    indager>=50 & indager<70 ~ "50-69",
    indager>=70 & indager<80 ~ "70-79",
    indager>=80  ~ "80+"
  )),
  cfmap.f=as_factor( cfmap,"both"),            
  cfmap2.f=as_factor( cfmap,"both"),            
  cfmap3.f=as_factor( cfmap,"both"),            
  cfmap3.fr=as.factor(case_when(
    cfmap==1 | cfmap==2 ~ "Excellent/v good",
    cfmap==3 ~ "Good",
    cfmap==4 ~ "Fair", 
    cfmap==5 ~ "Poor",
    .default = NA
  )),
  cfmap.fr=as.factor(case_when(
    cfmap==1 | cfmap==2 ~ "Excellent/v good",
    cfmap==3 ~ "Good",
    cfmap==4 | cfmap==5 ~ "Fair/poor",
    .default = NA
  )),
  cfmap2.fr=as.factor(case_when(
    cfmap==1 | cfmap==2 ~ "Excellent/v good",
    cfmap==3 | cfmap==4~ "Good/Fair",
    cfmap==5 ~ "Poor",
    .default = NA
  )),
  cfmetm.f=as_factor( cfmetm,"both"),            
  cfmetm2.f=as_factor( cfmetm,"both"),            
  cfmetm3.f=as_factor( cfmetm,"both"),            
  cfmetm3.fr=as.factor(case_when(
    cfmetm==1 | cfmetm==2 ~ "Excellent/v good",
    cfmetm==3 ~ "Good",
    cfmetm==4 ~ "Fair",  
    cfmetm==5 ~ "Poor",
    .default = NA
  )),
  cfmetm.fr=as.factor(case_when(
    cfmetm==1 | cfmetm==2 ~ "Excellent/v good",
    cfmetm==3 ~ "Good",
    cfmetm==4 | cfmetm==5 ~ "Fair/poor",
    .default = NA
  )),
  cfmetm2.fr=as.factor(case_when(
    cfmetm==1 | cfmetm==2 ~ "Excellent/v good",
    cfmetm==3 | cfmetm==4~ "Good/Fair",
    cfmetm==5 ~ "Poor",
    .default = NA
  )),
  digran.f=as_factor( digran,"both"),            
  digran.fr=as.factor(case_when(
    digran==1  ~ "Has gchild",
    digran==2 ~ "None"
  )),
  dimarr.f=as_factor( dimarr,"both"),            
  dimarr.fr=as.factor(case_when(
    dimarr==1  ~ "Single",
    dimarr==2 | dimarr==3 ~ "Married/SP",
    dimarr==4 | dimarr==5 ~ "Divorced/separated",
    dimarr==6 ~ "Widowed"
  )),
  edqual.f=as_factor( edqual,"both"),            
  edqual.fr=as.factor(case_when(
    edqual==1 | edqual==2 ~ "Further/Degree level",
    edqual==3 | edqual==4  | edqual==5 ~ "Secondary",
    edqual==6 ~ "Other",
    edqual==7 ~ "Below secondary"
  )),
  erlvolpe.f=as_factor( erlvolpe,"both"),            
  erlvolpe.fr=as.factor(case_when(
    erlvolpe==1  ~ "Cared for sick/frail",
    erlvolpe==0 ~ "Not a carer"
  )),
  erlvolba.f=as_factor( erlvolba,"both"),            
  erlvolba.fr=as.factor(case_when(
    erlvolba==0  ~ "No childcare",
    erlvolba==1 ~ "Did childcare"
  )),
  exlo90d.f=as.factor(
    ifelse(exlo90>50,"Yes","No")
  ),
  exlo90d.fr=exlo90d.f,
  exrela.f=as_factor( exrela,"both"),            
  exrela.fr=as.factor(case_when(
    exrela==1 |     exrela==2  ~ "Never/rarely",
    exrela==3 ~ "Sometimes",
    exrela==4 | exrela==5 ~ "Often/M of the T"
  )),
  hehelf.f=as_factor( hehelf,"both"),
  hehelf2.f=as_factor( hehelf,"both"),            
  hehelf3.f=as_factor( hehelf,"both"),            
  hehelf2.fr=as.factor(case_when(
    hehelf==1  | hehelf==2 ~ "Excel/V good",
    hehelf==3 | hehelf==4 ~ "Good/Fair",
    hehelf==5  ~ "Poor"
  )),
  hehelf.fr=as.factor(case_when(
    hehelf==1  | hehelf==2 ~ "Excel/V good",
    hehelf==3 ~ "Good",
    hehelf==4 | hehelf==5  ~ "Fair/poor"
  )),
  hehelf3.fr=as.factor(case_when(
    hehelf==1  | hehelf==2 ~ "Excel/V good",
    hehelf==3 ~ "Good",
    hehelf==4 ~ "Fair",
    hehelf==5  ~ "Poor"
  )),
  hhdtypb.f=as_factor( hhdtypb,"both"),            
  hhdtypb.fr=as.factor(case_when(
    hhdtypb==1  | hhdtypb==7 ~ "Single person HH",
    hhdtypb==2 | hhdtypb ==6 ~ "Two adults HH",
    hhdtypb==3 | hhdtypb==4 ~ "Family, large or small ",
    hhdtypb==5 ~ "Large adult HH",
    .default = "Other"
  )),
  iafcon.f=as_factor( iafcon,"both"),            
  iafcon.fr=as.factor(case_when(
    iafcon==1   ~ "Manage very well",
    iafcon==2   ~ "Manage quite well",
    iafcon==3   ~ "Get by alright",
    iafcon==4 | iafcon==5 | iafcon==6 ~ "Does not manage well"
  )),
  psagf.r=ifelse(psagf<0,NA,psagf),
  indager.r=ifelse(indager<0,NA,indager),
  AgeGap.r=indager.r-psagf.r,
  AgeGap31.f=as.factor(case_when(
    indager.r<psagf.r ~ "Feels older than age",
    indager.r==psagf.r ~ "Feels their age",
    indager.r>psagf.r ~ "Feels younger than age",
  )),
  AgeGap31.fr=AgeGap31.f,
  AgeGap32.f=as.factor(case_when(
    abs(AgeGap.r)<=3 ~ "Feels within 3 years of actual age",
    indager.r<(psagf.r+3) ~ "Feels > 3 years older",
    indager.r>(psagf.r+3) ~ "Feels > 3 years younger",
  )),
  AgeGap32.fr=AgeGap32.f,
  sclfcoa.f=as_factor( sclfcoa,"both"),            
  sclfcoa.fr=as.factor(case_when(
    sclfcoa==4 | sclfcoa==5 | sclfcoa==6  ~ "Give back to community",
    sclfcoa==1 | sclfcoa==2 | sclfcoa==3     ~ "Not so much",
  )),
  sptraa.f=as_factor( sptraa,"both"),            
  sptraa.fr=as.factor(case_when(
    sptraa==1 | sptraa==2  ~ "Several times a week/daily",
    sptraa==3 | sptraa==4     ~ "MT once a month",
    sptraa==5 | sptraa==6  ~ "Monthly or less"
  )),
  scorgch.f=as_factor( scorgch,"both"),            
  scorgch.fr=as.factor(case_when(
    scorgch==1   ~ "Member of a charity",
    scorgch==0     ~ "Not a member"
  )),
  tenureb.f=as_factor( tenureb,"both"),            
  tenureb.fr=as.factor(case_when(
    tenureb==1   ~ "Owner",
    tenureb==2 | tenureb==3 ~ "Mortgage",
    tenureb==4 |tenureb==5  ~ "Renter"
  )),
  w10nssec5.f=as_factor( w10nssec5,"both"),            
  w10nssec5.fr=as.factor(case_when(
    w10nssec5==1   ~ "Higher MAP",
    w10nssec5==2  ~ "Intermediate",
    w10nssec5==3 | w10nssec5==4  ~ "Small emp, lower superv.",
    w10nssec5==5  ~ "Semi-routine/routine"
  )),
  wpdes.f=as_factor(wpdes,"both"),            
  wpdes.fr=as.factor(case_when(
    wpdes==1 | wpdes==96 ~ "Retired (inc part)",
    wpdes==2 | wpdes==3~ "In employment",
    wpdes==4 | wpdes==6~ "FT carer + unemp.",
    wpdes==5 ~ "LT sick/disabled"
  ))            
  
)            



#### Voting record - 2001-2017 
for(l in c("VotGE01","VotGE05","VotGE15","VotGE17")){
  tmp<-as.numeric(
    eval(
      parse(
        text=paste0("elsaw$",l)
      )
    )
  )
  
  #    elsaw[paste0(l,".n")]<-ifelse(tmp<0 | is.na(tmp),0,tmp)  
  elsaw[paste0(l,".n")]<-ifelse(tmp<0,0,tmp)  
  
}

### Raw GE voting number
elsaw$votrec<-elsaw$VotGE01.n+elsaw$VotGE05.n+elsaw$VotGE15.n+elsaw$VotGE17.n

### Factor version, 3 cat 
elsaw<-elsaw|>mutate(
  VotRec2.fr=as.factor(case_when(
    votrec==0 ~ "None", 
    votrec==1 | votrec==2 | votrec==3 | votrec==4 ~ "Voted in the past")),
  VotRec0.fr=as.factor(case_when(
    votrec==0 ~ "None", 
    votrec==1 | votrec==2 ~ "1-2",
    votrec==3 | votrec==4 ~ "3-4")),
  VotRec01.fr=as.factor(case_when(
    votrec==0 | votrec==1~ "None or 1", 
    votrec==2 ~ "2",
    votrec==3 | votrec==4 ~ "3-4")
  ) )


### correcting a missing value in the label of VotGE05


attr(elsaw$VotGE05, "labels")<-c("Not answered","Item not applicable","No","Yes") 


for (hav in wvars  ) {
  elsaw<-cbind(elsaw,tmp=tfactor(
    eval(
      parse(
        text=paste0("elsaw$",hav)))))
  
  names(elsaw)[ncol(elsaw)]<-paste0(hav,".f")
}

### Issue with value labels (2)
levels(elsaw$VotGE05.f)<-c("[-9] Not answered","[-1] Item not applicable","[0] No","[1] Yes")

for(ov  in ovars) { ### Continuous vars exlo80 & exlo90 removed
  #nv<-paste0(ov,".fr")
  #ov<-paste0(v,".f")
  elsaw<-elsaw|>mutate(nv=
                         as.factor(eval(
                           parse(
                             text=paste0(  
                               "ifelse(as.numeric(",ov,")<0 | as.numeric(",ov,")>80,NA,",
                               ov,")"
                             )
                           )
                         )
                         )
  )
  names(elsaw)[ncol(elsaw)]<-paste0(ov,".fr")
}

levels(elsaw$VotGE19.fr)<-c("Didn't vote","Voted")
levels(elsaw$VotGE17.fr)<-c("Didn't vote","Voted")
levels(elsaw$VotGE15.fr)<-c("Didn't vote","Voted")
levels(elsaw$VotGE05.fr)<-c("Didn't vote","Voted")
levels(elsaw$VotGE01.fr)<-c("Didn't vote","Voted")





for(vr in names(labs[-19])){                        ### Leaving AgeCaat out
  attr(elsaw[vr],"label")<-labs[[vr]]
}


### Missing labels
levels(elsaw$scorgpo.fr)<-c("No", "Yes")
levels(elsaw$scorgrl.fr)<-c("No", "Yes")
levels(elsaw$erfvol.fr)<-names(attr(elsaw$erfvol, "labels")[4:5])
levels(elsaw$ethnic.fr)<-names(attr(elsaw$ethnic, "labels")[3:4])
levels(elsaw$headlba.fr)<-names(attr(elsaw$headlba, "labels")[4:5])
levels(elsaw$headlbe.fr)<-names(attr(elsaw$headlbe, "labels")[4:5])
levels(elsaw$hobb.fr)<-names(attr(elsaw$hobb, "labels")[4:5])
levels(elsaw$iafind.fr)<-names(attr(elsaw$iafind, "labels")[4:7])
levels(elsaw$indsex.fr)<-names(attr(elsaw$indsex, "labels")[4:5])
levels(elsaw$pscedd.fr)<-names(attr(elsaw$pscedd, "labels")[4:5])
levels(elsaw$pscede.fr)<-names(attr(elsaw$pscede, "labels")[4:5])
levels(elsaw$spcar.fr)<-names(attr(elsaw$spcar, "labels")[4:5])