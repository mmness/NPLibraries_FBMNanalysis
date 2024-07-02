




# summin ------------------------------------------------------------------
featlist<-read.csv("backgroundExclIn_quant.csv") 
node<-read.csv("node-backgroundEXINCL.csv")

#only want to keep the cluster and componet index from the node table
node<-cbind(node$cluster.index, node$componentindex)
colnames(node)<-c("cluster.index", "GNPSfamily" )
featlist_Comp<-merge(node, featlist , by.y = "row.ID", by.x = "cluster.index")

##singles removed. 
featlist_Comp_nosingles<- subset(featlist_Comp, GNPSfamily != "-1")
featlist_Comp_nosingles <- featlist_Comp_nosingles[, !(names(featlist_Comp_nosingles) %in% c("row.m.z", "row.retention.time", "X", "cluster.index"))]
featlist_Comp_nosingles$GNPSfamily <- paste("FAM", featlist_Comp_nosingles$GNPSfamily, sep = "")

summed_data_nosingles <- aggregate(featlist_Comp_nosingles[,-1], by = list(featlist_Comp_nosingles$GNPSfamily), FUN = sum)
colnames(summed_data_nosingles)<-sub("Group.1", "GNPSfamily",colnames(summed_data_nosingles) )


##singles not removed
nosumSing<-featlist_Comp

nosumSing$row.m.z<-round(nosumSing$row.m.z, 4)
nosumSing$row.retention.time<-round(nosumSing$row.retention.time, 2)
nosumSing$GNPSfamily <- paste("FAM", nosumSing$GNPSfamily, sep = "")

for (i in 1:nrow(nosumSing)) {
  if (nosumSing$GNPSfamily[i] == "FAM-1") {
    new_value <- paste("sing", nosumSing$row.m.z[i], nosumSing$row.retention.time[i], nosumSing$cluster.index[i], sep = "_")
    nosumSing$GNPSfamily[i] <- new_value
  }
}

nosumSing <- nosumSing[, !(names(nosumSing) %in% c("row.m.z", "row.retention.time", "X", "cluster.index"))]
nosumSing <- aggregate(nosumSing[,-1], by = list(nosumSing$GNPSfamily), FUN = sum)
colnames(nosumSing)<-sub("Group.1", "GNPSfamily",colnames(nosumSing) )

# Export ------------------------------------------------------------------
#write no singlets
write.csv(summed_data_nosingles, "nosingles.csv", row.names = FALSE)
#with singlets
write.csv(nosumSing, "withsingles.csv", row.names = FALSE)


# Qimeformatting-without singles ------------------------------------------
tab_nosing<-t(summed_data_nosingles)
rownames(tab_nosing) <- sub(".mzML.Peak.area|.mzXML.Peak.area", "", rownames(tab_nosing))
colnames(tab_nosing)<-tab_nosing[1,]
tab_nosing<-tab_nosing[-1,]
tab_nosing<-cbind(rownames(tab_nosing),tab_nosing)
colnames(tab_nosing)[1]<-"sampleid"
# Qiimeformating-with singlets --------------------------------------------
tab_sing<-t(nosumSing)
rownames(tab_sing) <- sub(".mzML.Peak.area|.mzXML.Peak.area", "", rownames(tab_sing))
colnames(tab_sing)<-tab_sing[1,]
tab_sing<-tab_sing[-1,]
tab_sing<-cbind(rownames(tab_sing),tab_sing)
colnames(tab_sing)[1]<-"sampleid"


# exporting ----------------------------------------------------------
write.table(tab_nosing, "table_nosingletsQfor.txt", row.names = FALSE, sep = "\t", na="NA") #make sure its .txt. NOT .csv
write.table(tab_sing, "table_withsinglets_Qfor.txt", row.names = FALSE, sep = "\t", na="NA") #make sure its .txt. NOT .csv

write.csv(tab_nosing, "table_nosingletsQfor.csv", row.names = FALSE, na="NA") 
write.csv(tab_sing, "table_withsinglets_Qfor.csv", row.names = FALSE, na="NA") 




#run through checking if needed