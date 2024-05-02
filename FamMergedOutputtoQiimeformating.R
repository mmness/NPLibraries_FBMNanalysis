table<-read.csv("GNPS_fammerge_nosingles_renormed.csv")

names(table) <- sub(".mzML.Peak.area", "", names(table))
trans<-t(table)
rownames(trans)[1] <- "sampleid"
trans[is.na(trans)] <- 0 

write.table(trans, "GNPS_fammerge_nosingles_renormed.txt", sep = "\t", col.names = FALSE, row.names = TRUE)



###################
meta<-read.table("postive_meta_edit.txt", sep = "\t", header = TRUE)
metasamps<-as.vector(meta$sampleid) 
featlist_samples<-row.names(trans)
unique_to_meta <- setdiff(metasamps, featlist_samples)
cat("Unique to Metadata file:", unique_to_meta, "\n")
unique_to_featlist <- setdiff(featlist_samples, metasamps)
cat("Unique to Featlist file:", unique_to_featlist, "\n")
