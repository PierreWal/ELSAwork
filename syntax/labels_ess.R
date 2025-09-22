# avars<-c("agea","AgeCat2.fr", "AgeCat3.fr","cfmap2","cfmap3", "cfmetm2", "cfmetm3", "dimarr", "edqual", "erfvol", "ethnic", "exrela", "headlba","headlbe","hehelf2","hehelf3", "hhdtypb","hobb","iafcon", "iafind","gndr", "pscedd", "pscede", "scorgpo","scorgrl","spcar",  "sptraa", "tenureb","vote","w10nssec5","wpdes")    


# avars<-c("AgeCat2.fr", "AgeCat3.fr","cfmap2","cfmap3", "cfmetm2", "cfmetm3", "dimarr", "edqual", "erfvol", "ethnic", "exrela", "headlba","headlbe","hehelf2","hehelf3", "hhdtypb","hobb","iafcon", "iafind","indsex", "pscedd", "pscede", "scorgpo","scorgrl","spcar",  "sptraa", "tenureb","VotGE01","VotGE05","VotGE15","VotGE17","VotGE19","VotRec2","VotRec0","VotRec01","w10nssec5","wpdes")    

# ovars<-c("erfvol", "ethnic", "headlba",  "headlbe","hobb", "iafind", "indsex","pscedd", "pscede", "scorgpo", "scorgrl","spcar", "VotGE01","VotGE05","VotGE15","VotGE17","VotGE19")    

ovars<-c("vote","hincfel", "fltlnl", "gndr", "happy", "sclmeet","rlgatnd",    "mbtru"  , "hswrk",  "tporgwk", "volunfp",   "polintr",  "netusoft")

exvars<-c("maritalb", "eisced", "health",  "hltprhb", "hltpral", "hltprbn", "hltprpa", "hltprpf","hltprsd", 
          "hltprsc", "hltprsh", "hltprdi","mnactic", "cntry") 
auxvars<-c("agea", "hhmmb","hltprhc", "hltprbp", "isco08", "rshpsts", "rshpsgb","rshipa2","rshipa3","rshipa4","rshipa5","rshipa6","rshipa7",
           "rshipa8","rshipa9","rshipa10","rshipa11","rshipa12","rshipa13")

desvars<-c("psu","anweight","stratum")

dvars<-c("AgeCat2", "AgeCat3","health2", "nrhltpb", "hhdtypb", "isco1")

uvars<-paste0(c(ovars,exvars,dvars),".f")
rvars<-paste0(c(ovars,exvars,dvars),".fr")

bivars19<-c(ovars[-1],exvars,dvars)

regvars1<-c("AgeCat3.fr","gndr.fr","maritalb.fr","hhdtypb.fr", "eisced.fr","mnactic.fr","cntry.fr")
reglab1<-list(AgeCat3.fr="Age",
                              gndr.fr="Sex",
                              maritalb.fr="Marital/relationship status",
                              hhdtypb.fr="Household type",
                              eisced.fr="Highest qualification",
                              mnactic.fr="Economic activity",
                              cntry.fr= "Country"
              )

ivars<-c("hltprhb.fr","health.fr","hincfel.fr", "fltlnl.fr", "happy.fr", "sclmeet.fr",
         "rlgatnd.fr",    "mbtru.fr"  , "hswrk.fr",  "tporgwk.fr", 
         "volunfp.fr",   "polintr.fr",  "netusoft.fr")


regvars2<-c(regvars1[-7],ivars)
reglab2<-list(AgeCat3.fr="Age",
              gndr.fr="Sex",
              maritalb.fr="Marital/relationship status",
              hhdtypb.fr="Household type",
              eisced.fr="Highest qualification",
              mnactic.fr="Economic activity",
              hltprhb.fr="Nr of health problems",
              health.fr="Self-rated health",
              hincfel.fr="Subj. financial situation",
              fltlnl.fr="Feels lonely?",
              happy.fr="Feels happy?",
              sclmeet.fr="Meet friends/relatives?",
              rlgatnd.fr="Religious attendence",    
              mbtru.fr="Trade-union etc member?", 
              hswrk.fr="Whether does caring/housework?",  
              tporgwk.fr="Organisation works/worked for", 
              volunfp.fr="Whether volunteers",   
              polintr.fr="Interest in politics",  
              netusoft.fr="Internet usage")

              


exvars<-c("maritalb", "eisced", "health",  "hltprhb", "hltpral", "hltprbn", "hltprpa", "hltprpf","hltprsd", 
          "hltprsc", "hltprsh", "hltprdi","mnactic", "cntry") 
auxvars<-c("agea", "hhmmb","hltprhc", "hltprbp", "isco08", "rshpsts", "rshpsgb","rshipa2","rshipa3","rshipa4","rshipa5","rshipa6","rshipa7",
           "rshipa8","rshipa9","rshipa10","rshipa11","rshipa12","rshipa13")

desvars<-c("psu","anweight","stratum")

dvars<-c("AgeCat2", "AgeCat3","health2", "nrhltpb", "hhdtypb", "isco1")


# wvars<-c("erfvol", "ethnic", "headlba","headlbe", "hobb", "iafind","indsex", "pscedd", "pscede", "scorgpo","scorgrl", "spcar",  "VotGE01","VotGE05","VotGE15","VotGE17","VotGE19")    




# 
# 
#       
# reglab2<-list(hobb.fr="Internet connection?",
#                               pscede.fr="Felt lonely", 
#                               hehelf3.fr="Self-rated general health", 
#                               iafcon.fr="How getting along financially",
#                               cfmetm3.fr="Self-rated mental health", 
#                               pscedd.fr="Happy last week?",
#                               sclfcoa.fr="Give back to community?", 
#                               erlvolpe.fr= "Unpaid carer",
#                               AgeGap.r="Perceived/actual age gap (cont)",
#                               AgeGap31.fr="Perceived/actual age gap (exact)",
#                               AgeGap32.fr="Perceived/actual age gap (+-3 years)",
#                               VotRec2.fr="Past GE turnout")
# 
# sdvars1<-c("AgeCat3.fr","indsex.fr","dimarr.fr","edqual.fr","wpdes.fr")
# sdvars2<-c("AgeCat3.fr","indsex.fr","dimarr.fr","edqual.fr","wpdes.fr","tenureb.fr")
# ivars<-c("hobb.fr","pscede.fr","hehelf3.fr","iafcon.fr","cfmetm3.fr","pscedd.fr","sclfcoa.fr","erlvolpe.fr","AgeGap32.fr")
# 
# regvars3<-c("VotRec2.fr",sdvars1,ivars)
# regvars4<-c("VotRec2.fr",sdvars2,ivars)
# 
# regvars5<-c(sdvars1)
# regvars6<-c(sdvars2)
# regvars7<-c(sdvars1,ivars)
# regvars8<-c(sdvars2,ivars)
