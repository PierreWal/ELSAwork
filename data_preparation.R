### Data preparation for ELSA recoding work


elsa0<-read_dta(paste0(datadir,"wave_0_common_variables_v2.dta"))
elsa.ifs<-read_dta(paste0(datadir,"wave_10_ifs_derived_variables.dta"))
elsa1<-read_dta(paste0(datadir,"wave_1_core_data_v3.dta")) |> select(idauniq,scptpa1,indager,indsex,w1wgt)
elsa3<-read_dta(paste0(datadir,"wave_3_elsa_data_v4.dta"))  |> select(idauniq,scpt01,indager,indsex,w3xwgt)
elsa8<-read_dta(paste0(datadir,"wave_8_elsa_data_eul_v2.dta")) |> select(idauniq,scptrvt, indager,indsex,w8xwgt)
elsa9<-read_dta(paste0(datadir,"wave_9_elsa_data_eul_v2.dta")) |> select(idauniq,scptrvt,indager,indsex,w9xwgt)
elsa10<-read_dta(paste0(datadir,"wave_10_elsa_data_eul_v4.dta"))

voted<-list(
GE01=elsa1,
GE05=elsa3,
GE15=elsa8,
GE17=elsa9,
GE19=elsa10 |> select(idauniq,scsptrvt,indager,indsex,w10xwgt)
)

names(voted[["GE01"]])[which(names(voted[["GE01"]])=="scptpa1")]<-"Wvot"
names(voted[["GE05"]])[which(names(voted[["GE05"]])=="scpt01")]<-"Wvot"
names(voted[["GE15"]])[which(names(voted[["GE15"]])=="scptrvt")]<-"Wvot"
names(voted[["GE17"]])[which(names(voted[["GE17"]])=="scptrvt")]<-"Wvot"
names(voted[["GE19"]])[which(names(voted[["GE19"]])=="scsptrvt")]<-"Wvot"


names(elsa1)[which(names(elsa1)=="scptpa1")]<-"VotGE01"
names(elsa3)[which(names(elsa3)=="scpt01")]<-"VotGE05"
names(elsa8)[which(names(elsa8)=="scptrvt")]<-"VotGE15"
names(elsa9)[which(names(elsa9)=="scptrvt")]<-"VotGE17"

for(w in c(1,3,8,9)){
tmp<-eval(parse(text=paste0("elsa",w)))
names(tmp)[3:4]<-c(paste0("indager",w),paste0("indsex",w))
eval(parse(text=paste0("elsa",w,"<-tmp")))
}

elsaw<-merge(elsa10,elsa0|>
                 select(idauniq,hhdtypb,tenureb,ethnic),
                 by="idauniq",all.x=T,all.Y=F)

elsaw<-merge(elsaw,elsa.ifs|>
                 select(idauniq,edqual),
                 by="idauniq",all.x=T,all.Y=F)

### Cross-sectional voting subsamples
vot1389<-merge(
     merge(elsa1,
           elsa3,
                 by="idauniq",all=T
           ),
      merge(elsa8 ,
            elsa9 ,
                 by="idauniq",all=T
           ),
           by="idauniq",all=T)


vot13890<-merge(vot1389,elsa10 |> select(idauniq,scsptrvt,indager,indsex,w10xwgt) ,
             by="idauniq",all=T
           )

names(vot13890)[which(names(vot13890)=="scsptrvt")]<-"VotGE19"
names(vot13890)[which(names(vot13890)=="indager")]<-"indager10"
names(vot13890)[which(names(vot13890)=="indsex")]<-"indsex10"

names(elsaw)[which(names(elsaw)=="scsptrvt")]<-"VotGE19"


elsaw<-merge(elsaw, vot1389,
             by="idauniq",all=T
           )


elsaw<-elsaw|>filter(!is.na(w10xwgt)) |>
           select(idauniq,all_of(ovars),exlo80, psagf, exrslf, indager, VotGE19,w10xwgt)

# elsaw<-elsaw|>filter(!is.na(w10xwgt)) |>
#            select(idauniq,all_of(ivs),exlo80, psagf, exrslf, indager, VotGE19,w10xwgt)

# Save the list to an RDS file
saveRDS(voted, file = "/home/mscsepw2/Dropbox/work/ELSAwork/data/voted.rds")

write_dta(vot13890, "/home/mscsepw2/Dropbox/work/ELSAwork/data/elsal.dta")
write_dta(elsaw, "/home/mscsepw2/Dropbox/work/ELSAwork/data/elsaw.dta")