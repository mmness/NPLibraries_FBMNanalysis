pairsTable<-read.csv("MERGE_NETWORKS_POLARITY-bc57e319-view_summary_table-main.tsv.csv")
negFeat<-read.csv("negative_featlist_osRemov_3xblank_3xcher.csv")
posFeat<-read.csv("postive_featlist_osRemov_5xblank_5xcher.csv")

#removing Blanks, QCs, 6mixs, and cherrio samples from pos and negative dfs
samps_to_remove <- "blank|QC|6mix|H9|H10|H11|H12"
negcolumns_to_remove <- grep(samps_to_remove, colnames(negFeat))
poscolumns_to_remove <- grep(samps_to_remove, colnames(posFeat))
negFeat_sampsonly <- negFeat[, -negcolumns_to_remove]
posFeat_sampsonly <- posFeat[, -poscolumns_to_remove]

#removing the features with pos/neg matchse from the NEGATIVE dataset. make sure scan# has negative titles
negfeatures_to_remove <- sub("negative-", "", pairsTable$scan2)
negFeat_sampsonly_dupremoved<- subset(negFeat_sampsonly, !(row.ID %in% negfeatures_to_remove))
nrow(negFeat_sampsonly)-nrow(negFeat_sampsonly_dupremoved) #10Kremoved
#removed ~10000 features. no big deal that its less because the GNPS merged job included the blanks, QCs, cheerios, etc. 
#many of those matched features probably got removed during the blank removal process

#now..I want the new feature names to have "pol_mz_rt_CI" format (EX. neg_120.1111_3.50_4321)
possampleid <-numeric()
for(i in 1:nrow(posFeat_sampsonly)){
  id<-c(posFeat_sampsonly$row.m.z[i], posFeat_sampsonly$row.retention.time[i], posFeat_sampsonly$row.ID[i])
  nums<-c("Xpos", paste(id, collapse = "_"))
  possampleid[i]<-paste(nums, collapse = "")
}
posFeat_sampsonly_wID<-cbind(possampleid, posFeat_sampsonly[, -(1:3)])

negsampleid <-numeric()
for(i in 1:nrow(negFeat_sampsonly_dupremoved)){
  id<-c(negFeat_sampsonly_dupremoved$row.m.z[i], negFeat_sampsonly_dupremoved$row.retention.time[i], negFeat_sampsonly_dupremoved$row.ID[i])
  nums<-c("Xneg", paste(id, collapse = "_"))
  negsampleid[i]<-paste(nums, collapse = "")
}
negFeat_sampsonly_dupremoved_wID<-cbind(negsampleid, negFeat_sampsonly_dupremoved[, -(1:3)])

#Qiime2-ese format. 
names(negFeat_sampsonly_dupremoved_wID) <- sub(".mzML.Peak.area", "", names(negFeat_sampsonly_dupremoved_wID))
negtrans<-t(negFeat_sampsonly_dupremoved_wID)
rownames(negtrans)[1] <- "sampleid"
negtrans<-cbind(row.names(negtrans), negtrans)
colnames(negtrans)<-negtrans[1,]
negtrans<-negtrans[-1,]
negtrans[is.na(negtrans)] <- 0     #this replaces any "NA" output with zero 

names(posFeat_sampsonly_wID) <- sub(".mzML.Peak.area", "", names(posFeat_sampsonly_wID))
postrans<-t(posFeat_sampsonly_wID)
rownames(postrans)[1] <- "sampleid"
postrans<-cbind(row.names(postrans), postrans)
colnames(postrans)<-postrans[1,]
postrans<-postrans[-1,]
postrans[is.na(postrans)] <- 0     #this replaces any "NA" output with zero 

###changing names so they can match and be merged. plate_well# format
postrans<-as.data.frame(postrans)
negtrans<-as.data.frame(negtrans)

postrans$sampleid<-gsub("_blue|_red|_green|_pos","", postrans$sampleid)
negtrans$sampleid<-gsub("_blue|_red|_green|_neg", "", negtrans$sampleid)

new_row_names_pos <- sub("_\\d+$", "", postrans$sampleid)
postrans$sampleid <- new_row_names_pos

new_row_names_neg <- sub("_\\d+$", "", negtrans$sampleid)
negtrans$sampleid <- new_row_names_neg

#checking to see the names match
possamps<-as.vector(postrans$sampleid)
negsamps<-as.vector(negtrans$sampleid) 

unique_to_pos <- setdiff(possamps, negsamps)
cat("Unique to pos file:", unique_to_pos, "\n") #none 
unique_to_neg <- setdiff(negsamps, possamps)
cat("Unique to neg file:", unique_to_neg, "\n")   #3
#these 3 are getting removed from repective files. probably missed inject cut offs or removed data
negtrans <- negtrans[!(negtrans$sampleid %in% c("p254_G2_2596...Copy", "p258_B7", "p248_B1_1174" )), ]

############MERGETIME
mergeddf<-merge(postrans, negtrans, by="sampleid")
write.csv(mergeddf, "merged_data.csv", row.names = FALSE)





