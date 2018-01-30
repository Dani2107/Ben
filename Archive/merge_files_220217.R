setwd("~/Desktop/Bouts")
file_list<-list.files("~/Desktop/Bouts")
file_list
require(data.table)
allbouts = rbindlist(lapply( file_list, fread, header=TRUE, sep="\t"))

write.table(allbouts,"~/Desktop/allbouts.txt",sep="\t",row.names=FALSE)
