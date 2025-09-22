############################################################################################
## 
## 


labs<-list(
eisced.f="Educational level",
cntry.f="Country",
#  ethnic.f="White vs Non-white",
fltlnl.f="Loneliness",
gndr.f="Sex",
happy.f="Happiness",
health.f="Self-reported general health V1",
health2.f="Self-reported general health V2",
hhdtypb.f="Recoded household type",
hincfel.f="Subjective financial situation",
hltprhc.f="Heart or circulation problem",
hltprhb.f="High blood pressure",
hltprbp.f="Breathing problems",
hltpral.f="Allergies",
hltprbn.f="Back or neck pain",
hltprpa.f="Muscular or joint pain in hand or arm",
hltprpf.f="Muscular or joint pain in foot or leg",
hltprsd.f="Stomach or digestion related",
hltprsc.f="Skin condition related",
hltprsh.f="Severe headaches",
hltprdi.f="Diabetes",
hswrk.f="Whether doing house- or care work",
isco1.f="Occupation, ISCO08 1 digit",
maritalb.f="Marital/relationship status",
mbtru.f="Trade union or similar membership",
mnactic.f= "Economic activity",
netusoft.f="Internet usage",
nrhltpb.f="Number of reported health problems",
polintr.f="Interest for politics",
rlgatnd.f="Religious attendence",
sclmeet.f="Social life intensity",
tporgwk.f= "Type of organisation working/worked for",
volunfp.f="Whether volunteered",
vote.f="Whether voter at the last GE",
AgeCat2.fr="Recoded age, 2 category",
AgeCat3.fr="Recoded age, 3 category"
)


## Recode of relationship to person nr1
for(nr in 2:13){
  ess[,ncol(ess)+1]<-ifelse(ess[paste0("rshipa",nr)]!=2 | is.na(ess[paste0("rshipa",nr)]),0,1)
  names(ess)[ncol(ess)]<-paste0("nrshipa",nr)
}

##########################################################


ess<-ess|>mutate(
  hasprt=ifelse(hhmmb>1 & (rshipa2==1 | rshipa3==1 | rshipa4==1 | 
                             rshipa5==1 | rshipa6==1 | rshipa7==1
                           | rshipa8==1 | rshipa9==1 | rshipa10==1
                           | rshipa11==1 | rshipa12==1 | rshipa13==1),
                "Cohab", "No cohab" ),
  haschi=ifelse(hhmmb>1 & (rshipa2==2 | rshipa3==2 | rshipa4==2 | 
                             rshipa5==2 | rshipa6==2 | rshipa7==2
                           | rshipa8==2 | rshipa9==2 | rshipa10==2
                           | rshipa11==2 | rshipa12==2 | rshipa13==2), 
                "Kids", "No kids" ),
  hincfel.f=as_factor(hincfel, "both"),
  hincfel.fr=as.factor(ifelse(!is.na(hincfel),hincfel,NA)),     
    fltlnl.f=as_factor(fltlnl, "both"),
  fltlnl.fr=as.factor(case_when(fltlnl<=1 ~ "None/almost none of the time",
                                fltlnl== 2  ~ "Some of the time",
                                fltlnl== 3 | fltlnl== 4  ~ "Most/all of the time")
  ),
  cntry.f=as.factor(cntry),
  cntry.fr=as.factor(cntry),
  gndr.f=as_factor(gndr, "both"),
  gndr.fr=droplevels(as_factor(gndr, "both")),
    nrchi=(nrshipa2+nrshipa3+nrshipa4+
           nrshipa5+nrshipa6+nrshipa7+
           nrshipa8+nrshipa9+nrshipa10+
           nrshipa11+nrshipa12+nrshipa13),
  hhdtypb.f=as.ordered(case_when(
    hhmmb==0 | hhmmb==1 ~ "Single person HH",  
    hhmmb==2 & rshipa2==1 ~"Two persons (couple) HH",
    hhmmb==(nrchi+1) & nrchi>0 ~"Single parent",   
    hhmmb==nrchi+2 & hasprt=="Cohab" ~"Nuclear family",
    .default="Other"
  )),
  hhdtypb.fr=as.factor(case_when(
    hhmmb==0 | hhmmb==1 ~ "Single person HH",  
    hhmmb==2 & rshipa2==1 ~"Two persons (couple) HH",
    hhmmb==(nrchi+1) & nrchi>0 ~"Single parent",   
    hhmmb==nrchi+2 & hasprt=="Cohab" ~"Nuclear family",
    .default="Other"
  )),
  happy.f=factor(as_factor(happy),ordered=T),
  happy.fr=as.factor(case_when(happy<6 ~ "1st quartile - least happy",
                               happy== 6 | happy==7 ~ "2nd qtile",
                               happy== 8   ~ "3rd qtile",
                               happy== 9 | happy==10  ~ "4th qtile")
  ),
  sclmeet.f=as_factor(sclmeet,"both"),
    sclmeet.fr=as.factor(case_when(sclmeet<=3 ~ "Monthly or less",
                                 sclmeet== 4 | sclmeet== 5 ~ "Weekly or less",
                                 sclmeet== 6 | sclmeet== 7  ~ "Daily or less")
  ),
  rlgatnd.f=as_factor(rlgatnd,"both"),
    rlgatnd.fr=as.factor(case_when(rlgatnd<=3 ~ "Once a week or more",
                                   rlgatnd== 4 | rlgatnd== 5 ~ "Less than weekly",
                                   rlgatnd== 6 | rlgatnd== 7  ~ "Rarely or never")
  ),
  isconum=as.integer(isco08/1000),
  isco1.f=as.factor(case_when(
    isconum==0 ~ "0-Armed Forces",
    isconum==1 ~"1-Managers",   
    isconum==2 ~"2-Professionals", 
    isconum==3 ~ "3-Technicians and Associate Professionals",
    isconum==4 ~  "4-Clerical Support Workers", 
    isconum==5 ~ "5-Service and sales",
    isconum==6 ~ "6-Skilled Agricultural, Forestry and Fishery Workers", 
    isconum==7 ~ "7-Craft and Related Trades Workers",
    isconum==8 ~ "8-Plant and Machine Operators, and Assemblers", 
    isconum==9 ~ "9-Elementary Occupations"
  )),
  isco1.fr=as.factor(case_when(
    isconum==1 ~"1-Managers",   
    isconum==2 ~"2-Professionals", 
    isconum==3 ~ "3-Technicians and Associate Professionals",
    isconum==4 | isconum==5  ~  "4+5-Clerical Support, Service and sales",
    isconum==6 | isconum==7~ "6+7-Skilled Agric., forest., fishery, craft and related",
    isconum==8 ~ "Plant and Machine Operators, and Assemblers", 
    isconum==9 ~ "Elementary Occupations"
  )),
  AgeCat2.f=as.factor(case_when(
    agea>=50 & agea<70 ~ "50-69",
    agea>=70 ~ "70+"
  )),
    AgeCat2.fr=as.factor(case_when(
    agea>=50 & agea<70 ~ "50-69",
    agea>=70 ~ "70+"
  )),
  AgeCat3.f=as.factor(case_when(
    agea>=50 & agea<70 ~ "50-69",
    agea>=70 & agea<80 ~ "70-79",
    agea>=80  ~ "80+"
  )),
  AgeCat3.fr=as.factor(case_when(
    agea>=50 & agea<70 ~ "50-69",
    agea>=70 & agea<80 ~ "70-79",
    agea>=80  ~ "80+"
  )),
    maritalb.f=as_factor( maritalb,"both"),            
  maritalb.fr=as.factor(case_when(
    maritalb==6  ~ "Single",
    maritalb==1 | maritalb==2 | rshpsts<=4 | rshpsgb<4 ~ "Married/SP",
    maritalb==3 | maritalb==4 ~ "Divorced/separated",
    maritalb==5  ~ "Widowed"
  )),
  mbtru.f=as_factor(mbtru,"both"),
  mbtru.fr=as.factor(ifelse(!is.na(mbtru),mbtru,NA)),     
    eisced.f=as_factor( eisced,"both"),            
  eisced.fr=as.factor(case_when(
    eisced==5 | eisced==6 | eisced==7 ~ "Further/Degree level",
    eisced==2 | eisced==3  | eisced==4 ~ "Secondary",
    eisced==0 | eisced==55~ "Other",
    eisced==1 ~ "Below secondary"
  )),
  hswrk.f=as_factor( hswrk,"both"),            
  hswrk.fr=as.factor(case_when(
    hswrk==1  ~ "Did housework/cared for someone ",
    hswrk==0 ~ "Did not report"
  )),
  health.f=as_factor( health,"both"),
  health.fr=as.factor(case_when(
    health==1  | health==2 ~ "Very good/good",
    health==3 ~ "Fair",
    health==4 | health==5  ~ "Poor"
  )),
  health2.f=as_factor( health,"both"),            
  health2.fr=as.factor(case_when(
    health==1  | health==2 | health==3 ~ "Not bad",
    health==4 | health==5  ~ "Bad or very bad"
  )),
  hltprhc.f=as_factor( hltprhc,"both"),
    hltprhb.f=as_factor( hltprhb,"both"),
  hltprbp.f=as_factor( hltprbp,"both"),
    hltpral.f=as_factor( hltpral,"both"),
  hltprbn.f=as_factor( hltprbn,"both"),
  hltprpa.f=as_factor( hltprpa,"both"),
  hltprpf.f=as_factor( hltprpf,"both"),
  hltprsd.f=as_factor( hltprsd,"both"),
  hltprsc.f=as_factor( hltprsc,"both"),
  hltprsh.f=as_factor(hltprsh,"both"),
  hltprdi.f=as_factor(hltprdi,"both"),
  hltprhc.fr=as_factor( hltprhc,"both"),
  hltprhb.fr=as_factor( hltprhb,"both"),
  hltprbp.fr=as_factor( hltprbp,"both"),
  hltpral.fr=as_factor( hltpral,"both"),
  hltprbn.fr=as_factor( hltprbn,"both"),
  hltprpa.fr=as_factor( hltprpa,"both"),
  hltprpf.fr=as_factor( hltprpf,"both"),
  hltprsd.fr=as_factor( hltprsd,"both"),
  hltprsc.fr=as_factor( hltprsc,"both"),
  hltprsh.fr=as_factor(hltprsh,"both"),
  hltprdi.fr=as_factor(hltprdi,"both"),
  nrhltpb.f=hltprhc+  hltprhb+  hltprbp+  hltpral+  hltprbn+
  hltprpa+  hltprpf+  hltprsd+  hltprsc+  hltprsh+ hltprdi,
  nrhltpb.fr=as.ordered(case_when(nrhltpb.f==0 ~"None",
                                nrhltpb.f==1 ~ "One" ,
                                nrhltpb.f==2 ~ "Two" ,
                                nrhltpb.f==3 | nrhltpb.f==4 ~ "3-4" ,
                                nrhltpb.f>=5 ~ "5+")) ,
  tporgwk.f=as_factor(tporgwk,"both"),
  tporgwk.fr=as.factor(case_when(
    tporgwk==1   ~ "Government (inc. local)",
    tporgwk==2 ~ "Other public",
    tporgwk==3 ~ "State-owned",
    tporgwk==4 ~ "Private sector",
    tporgwk==5 ~ "Self-employed",
    tporgwk==6 | tporgwk==7~ "Other/DNA"
  )),
  volunfp.f=as_factor(volunfp,"both"),
  volunfp.fr=as.factor(ifelse(!is.na(volunfp),volunfp,NA)),     
    vote.f=as_factor(vote,"both"),
    vote.fr=as.ordered(case_when(
      vote==1   ~ "Voted",
      vote==2 | vote==3  ~ "Did not vote"
  )),
  polintr.f=as_factor(polintr,"both"),            
  polintr.fr=as.factor(case_when(
    polintr==1  | polintr==2 ~ "Interested in politics",
    polintr==3 | polintr==4  ~ "Not interested in politics"
  )),
  netusoft.f=as_factor(netusoft,"both"),            
  netusoft.fr=as.factor(case_when(
    netusoft==1   ~ "Never",
    netusoft==2 | netusoft==3  ~ "Occasionally/ a few times a week",
    netusoft==4 | netusoft==5  ~ "Most days/ every day"
  )),
  mnactic.f=as_factor(mnactic,"both"),            
      mnactic.fr=as.factor(case_when(
    mnactic==6  ~ "Retired (inc part)",
    mnactic==1 ~ "In employment",
    mnactic==8  ~ "FT carer",
    mnactic==5 ~ "LT sick/disabled",
    mnactic==2 | mnactic==3 | mnactic==4 |
      mnactic==7 | mnactic==9 ~ "Ec. inactive"
  ))            
  
)            


levels(ess$hincfel.fr)<-levels(as_factor(ess$hincfel))[1:4]
levels(ess$mbtru.fr)<-levels(as_factor(ess$mbtru))[1:3]
levels(ess$volunfp.fr)<-levels(as_factor(ess$volunfp))[1:2]


# 
# ### correcting a missing value in the label of VotGE05
# 
# 
# for (hav in wvars  ) {
#   ess<-cbind(ess,tmp=tfactor(
#     eval(
#       parse(
#         text=paste0("ess$",hav)))))
#   
#   names(ess)[ncol(ess)]<-paste0(hav,".f")
# }
# 
### Issue with value labels (2)
# levels(ess$VotGE05.f)<-c("[-9] Not answered","[-1] Item not applicable","[0] No","[1] Yes")

# for(ov  in ovars) { ### Continuous vars exlo80 & exlo90 removed
#   #nv<-paste0(ov,".fr")
#   #ov<-paste0(v,".f")
#   ess<-ess|>mutate(nv=
#                          as.factor(eval(
#                            parse(
#                              text=paste0(  
#                                "ifelse(as.numeric(",ov,")<0 | as.numeric(",ov,")>80,NA,",
#                                ov,")"
#                              )
#                            )
#                          )
#                          )
#   )
#   names(ess)[ncol(ess)]<-paste0(ov,".fr")
# }


for(vr in names(labs[-19])){                        ### Leaving AgeCaat out
  attr(ess[vr],"label")<-labs[[vr]]
}


### Missing labels
# levels(ess$scorgpo.fr)<-c("No", "Yes")
# levels(ess$rlgatnd.fr)<-c("No", "Yes")
# levels(ess$volunfp.fr)<-names(attr(ess$volunfp, "labels")[1:2])
# levels(ess$ethnic.fr)<-names(attr(ess$ethnic, "labels")[3:4])
# levels(ess$headlba.fr)<-names(attr(ess$headlba, "labels")[4:5])
# levels(ess$headlbe.fr)<-names(attr(ess$headlbe, "labels")[4:5])
# levels(ess$hobb.fr)<-names(attr(ess$hobb, "labels")[4:5])
# levels(ess$iafind.fr)<-names(attr(ess$iafind, "labels")[4:7])
# levels(ess$gndr.fr)<-names(attr(ess$gndr, "labels")[1:2])
# #levels(ess$happy.fr)<-names(attr(ess$happy, "labels")[4:5])
# levels(ess$pscede.fr)<-names(attr(ess$pscede, "labels")[4:5])
# levels(ess$spcar.fr)<-names(attr(ess$spcar, "labels")[4:5])