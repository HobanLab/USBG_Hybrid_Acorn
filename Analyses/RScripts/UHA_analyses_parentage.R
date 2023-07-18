#####################
#     Libraries     #
#####################

library(geosphere)

###############################
#     Parentage Analyses      #
###############################

#set working directory 
setwd("G:/Shared drives/Emily_Schumacher/Project_Folders/USBG_Hybrid_Acorns/UHA_analyses")

#load in data file 
UHA_results_df <- read.csv("UHA_results_df.csv")

#clean data frame 
UHA_results_df <- UHA_results_df[,1:10]

##calculate distances
#create a column for distance between mom and dad 
UHA_results_df$distance_between_parents <- NA

#loop to calculate distance between parents
for(d in 1:nrow(UHA_results_df)){
  
  UHA_results_df$distance_between_parents[d] <- distm(UHA_results_df[d,7:8], 
                                                      UHA_results_df[d,9:10],
                                                      fun=distGeo)
  
}

#calculate mean distance between parents 
UHA_dist_matrix <- matrix(nrow = length(unique(UHA_results_df$Mother_ID)),
                          ncol = 1)
for(m in 1:length(unique(UHA_results_df$Mother_ID))){
  
  UHA_dist_matrix[m,1] <- mean(UHA_results_df[UHA_results_df$Mother_ID == unique(UHA_results_df$Mother_ID)[[m]],][,11])

}

#name matrix 
rownames(UHA_dist_matrix) <- unique(UHA_results_df$Mother_ID)
colnames(UHA_dist_matrix) <- "Mean_Dist_Parents"

##plot different distances between parents 
pdf("dist_parents.pdf", width = 8, height = 8)
boxplot(UHA_results_df$distance_between_parents~UHA_results_df$Mother_ID,
        ylim = c(0,600), xlab = "Maternal Individual",
        ylab = "Distance between Parents")
dev.off()



