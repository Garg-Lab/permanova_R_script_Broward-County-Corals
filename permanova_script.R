install.packages("vegan")
require(vegan)

#reads feature table
features <- read.csv(".../SCTLD_features.csv", sep = ',', header = TRUE, row.names = 1)

#reads sample table
samples <- read.csv("SCTLD_samples.csv", sep = ',', header = TRUE)

#displays the internal structure of feature table
str(features)

#displays the internal structure of sample table
str(samples)

#computes distance matrix using Bray-Curtis distances. 
#feature intensities are transformed by a factor of 0.25. 
dist_SCTLD <- vegdist(features^0.25, method = 'bray')

#generates the NMDS ordination
nmds <- metaMDS(dist_SCTLD)

#generates NMDS plot
plot(nmds)

#labels the NMDS plot
plot(nmds, type = 'text')

#performs PERMANOVA test on transformed feature intensities, using 999 permutations and Bray-Curtis distances
pmv <- adonis(features^0.25 ~ condition, data = samples, permutations = 999, method = 'bray')
pmv

#generates the density plot
densityplot(permustats(pmv))

