## Data Collection

#1. We start by downloading the SLE B cell dataset from Garaud et al. This dataset compares the transcript profiles of peripheral B lymphocytes between SLE patients and healthy controls.

#2. We use R to read the data from the downloaded text file into a data frame. 


library(ggplot2)

#Read the data file into a data frame
data <- read.table("sle_b_cell.txt", header = TRUE, row.names = 1)

df <- read.table("sle_b_cell.txt", header = TRUE)


#We examine the dimensions of the data to ensure that we have 26 samples.

#Set row names and remove the first column
rownames(df) <- df$ID_REF
df <- df[, -1]

#Check the dimensions and remove row names
dim(data)
rownames(data) <- NULL


#We print the sample names to the screen.

sample_names <- colnames(data)
print(sample_names)


##Data Visualization

#We create an XY scatter plot comparing the second SLE patient sample with the first normal control sample.

ggplot(data, aes(x = control.1, y = sle.1)) + geom_point() +
  labs(x = 'Normal', y = 'SLE', 
       title = 'SLE B cell sample vs. Normal B cell sample - all probesets') + 
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"))


#Create a similar scatter plot for the first 20 probesets.

ggplot(data[1:20, ], aes(x = control.1[1:20], y = sle.2[1:20])) + geom_point(pch = 15, color = "blue") + 
  labs(x = "Normal", y = "SLE", 
       title = "SLE B cell sample vs. Normal B cell sample - First 20 probesets") +
  theme_minimal()

#Visualize the gene profile for the gene "IGLJ3" (Probeset 211881_x_at).

gene_id <- "211881_x_at"
gene_data <- as.numeric(df[gene_id,])
gene_profile <- data.frame(Sample = 1:length(gene_data), Intensity = gene_data)

ggplot(gene_profile, aes(x = Sample, y = Intensity)) +
  geom_line() +
  labs(x = "Sample Index", y = "Intensity", title = "Gene Profile for IGLJ3 (Probeset 211881_x_at)") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"))


#Create a boxplot to visualize the distribution of IGLJ3 expression by disease or normal condition.

library(ggplot2)
f <- factor(c(rep("SLE", 17), rep("Control", 9)))

ggplot(data.frame(Condition = f, Intensity = gene_data), aes(x = Condition, y = Intensity)) + 
  geom_boxplot(color = "red", fill = "orange", alpha = 0.2) + 
  labs(x = "Condition", y = "Intensity", title = "Boxplot of IGLJ3 by Condition") + 
  theme_minimal()



