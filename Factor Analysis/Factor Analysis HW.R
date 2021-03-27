#FactorAnalysis

install.packages("nFactors")
Personality= read.csv(file = "PersonalityData.xls")
library(corrplot)
corrplot(cor(Personality[,2:12]), order = "hclust")#We see from this plot that the test scores group into two clusters of highly correlated variables: The size of the circles in the graph above represents strength of correlation between pairs of variables.
eigenval=eigen(cor(Personality[,2:12]))#the eigenvalue > 1 greater than 1 criterion
plot(eigenval$values, main="Scree Plot", type="l")
#The scree plot also suggests 3 factors. (This is a plot of the eigenvalues that are associated with factors ) The figure indicates where additional factors do not add much to explained variance. There is an elbow in the figure at factor 4. This indicates that 3 factors are sufficient in capturing the variation in the data.
library(nFactors)
nScree(Personality[, 2:12], cor=TRUE) #This function help you determine the number of factors
#The Kaiser criterion (nkaiser) corresponds to the eigenvalue > 1 criterion when factor analysis is applied on the correlation matrix. The acceleration factor criterion (naf) is based on the elbow criterion in the scree plot. Parallel analysis (nparallel) and optimal coordinates (noc) are two other methods. As you can see four criteria indicated 3 factors to retain. Now we are ready to run a factor analysis with 3 factors using the Varimax Rotated Principal Components method.
library(psych)
#We use the factor loadings to interpret the factors. The factor loadings are the correlations between the variables (i.e., the tests scores) and the factors. In interpreting each factor, underline the variables that load high (i.e., has a high correlation) on the factor and ask what’s common between these variables.
fit$loadings# print the factor loading results
fit <- principal(Personality[,2:12], nfactors=5, rotate="none")#without Varimax rotation), to see how much variance it is explained as we extract varying number of factors. We do this analysis below.
fit$loadings

fit <- principal(Personality[, 2:12],nfactors=3, rotate="varimax")  #How do you interpret the RC3 factor? Extrovert/introvert
fit$values#How do you interpret the RC2 factor?Dependable/Non dependable
# how do u interpret the RC1 factor? Amxious/non anxious
#print the eigen values of the correlation matrix
#Interpreting the two factors
colnames(fit$weights) = c("Amxious/non anxious", "Dependable/Non dependable", "Extrovert/introvert") # Naming the factors
fit$weights
#the factor weights show how the factor scores are obtained. For example, the scores on factor 1 are obtained as follows: Amxious/non anxious=0.11talkative  + 0.04880thorough 
#Note that all the attribute ratings are standardized to mean zero and unit variance when computing the factor scores. In such a case, each factor will also be standardized.
colnames(fit$scores) = c("Amxious/non anxious", "Dependable/Non dependable", "Extrovert/introvert") 
#reduced_data = cbind(Personality[,1:3],fit$scores)
#head(reduced_data)
#attach(reduced_data)
#x_mean=aggregate(reduced_data[,c(4:6)], by=list(X), FUN=mean, na.rm=TRUE)
#detach(reduced_data)
options(scipen = 200)
#head(x_mean)
head(fit$scores )#Examine the factor $scores for subject 1. How do you describe this person?Slightly non-anxious, dependable, slightly extrovert 
####

###perceptual map
colnames(fit$scores) = c("Verbal", "Math") # renaming RC1 as "Verbal" factor and RC2 as "Math" factor
biplot(fit$scores, fit$loadings, xlabs=test_scores[,1], main = "Perceptual Map\n\n") # Perceptual map
abline(h=0) #add a horizontal line in the graph
abline(v=0) #add a vertical line in the graph
# Varimax Rotated Principal Components
# retaining 2 factors 
library(psych)
fit <- principal(test_scores[, 2:7],nfactors=2, rotate="varimax")
#extracting scores and loadings info from "fit" object, and binding them together in order to plot together
ty = rbind(as.data.frame(unclass(fit$scores)),
           (as.data.frame(unclass(fit$loadings))*3)) #multiplied by scalar for plot 
colnames(ty) = c("Verbal", "Math")
#Credit for the plotting function goes to Ben Levine, Ph.D student at Columbia Business School
library(ggplot2) #library for creating visualizations

#PCA plot!
#ggplot takes at least two arguments: what dataframe it is referencing (ty),
#and the aesthetic mappings of the plot it will produce, i.e. the names of the columns it will use for the x and y axes.
#the default x and y axes titles are the variable names (in this case, RC1 and RC2). if you want to change them just uncomment the below:
# colnames(ty) = c("yournew_x_name", "yournew_y_name") 
library(ggplot2)
library(ggrepel)
ggplot(ty, aes(Verbal, Math)) +
  #first, the scores  
  
  #for the students, we will restrict the referenced data to only the first 10 rows of ty (the last 6 are classes).
  #rather than plotting points ('geom_point()'), we are plotting the text, hence the use of the geom_text function
  geom_text(data = ty[1:10,],
            aes(x = ty[1:10,1],
                y = ty[1:10,2],
                label = rownames(ty[1:10,]))) + #assigning the row names as the text labels, but could be any list of 10 character elements 
  #the 'sec.axis' arguments below create a second x and y axis to mimic the BI plot above,
  #the creators of R and the ggplot package intentionally try to discourage plots with more than 2 axes on the same plane
  #so we must make the "second" axes linear transformations of the first. here, i divide by 3 to mimic the BI plot above
  #i had to multiply the loadings data by 3 when i joined it to the scores data,
  #because plotting the raw loadings data on the 'scores' scale made the arrows too small. again, this is made intentionally difficult
  #we can change the second axes titles by changing what's given to the 'name' argument
  scale_y_continuous(limits = c(-3,3),
                     sec.axis = sec_axis(~./3, name = "Math")) +
  scale_x_continuous(limits = c(-3,3),
                     sec.axis = sec_axis(~./3, name = "Verbal")) +
  #the below is purely to make the plot look better, setting colors, typeface, etc.
  theme_classic() +
  theme(axis.text.x.top = element_text(color = "red"),
        axis.text.y.right =  element_text(color = "red"),
        plot.title = element_text(face = "bold",hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = .4) +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = .4) +
  
  #now we do loadings! referencing a subset of row vectors that correspond to the loadings data
  
  #plotting 'segments' as the line one can draw between the origin and the point
  geom_segment(data=ty[11:16,],
               aes(x = ty[11:16,1],
                   y =ty[11:16,2]),
               xend=0,
               yend=0,
               arrow = arrow(length = unit(0.03, "npc"),
                             ends = "first"),
               color = "red") + 
  #'smart labels' that won't overlap with each other
  geom_label_repel(data=ty[11:16,],
                   aes(x = ty[11:16,1],
                       y = ty[11:16,2],
                       label = rownames(ty[11:16,])),
                   label = rownames(ty[11:16,]),
                   segment.alpha = .25,
                   box.padding = unit(0.35, "lines"),
                   segment.color = "grey50") +
  #setting a title
  ggtitle("Perceptual Map\n")
#and, finally, saving to your working directory
ggsave("pca_plot_example.png", height = 8, width = 8)


#######3d example ########
library(scatterplot3d)
s3d <- scatterplot3d(Appearance, Performance, Comfort,
                     scale.y = 1, type='h', asp = 1,
                     main="3D Perceptual Map")
text(s3d$xyz.convert(Appearance, Performance, Comfort + c(rep(0.05,5),0.1)),
     labels=(brand.mean[,1]), 
     col = 'red')
library(scatterplot3d)
s3d1 <- scatterplot3d(brand_by_edu[,3], brand_by_edu[,4], 
                      brand_by_edu[,5], xlab = "Appearance", 
                      ylab = "Performance", zlab = "Comfort",
                      scale.y = 1, type='h', asp = 1,
                      main="3D Perceptual Map")
tmp <- brand_by_edu[which(brand_by_edu$Edu == 'MBA'),]
text(s3d1$xyz.convert(tmp$Appearance, tmp$Performance, 
                      tmp$Comfort + c(rep(0.05,5),0.1)),
     labels=(tmp$Brands), col = 'darkgreen')
tmp <- brand_by_edu[which(brand_by_edu$Edu=='Undergrad'),] 
text(s3d1$xyz.convert(tmp$Appearance, tmp$Performance, 
                      tmp$Comfort + c(rep(0.05,5),0.1)),
     labels=(tmp$Brands), col = 'red')
legend(-3, 8, 
       legend=c("MBA", "Undergrad"),
       col=c("red", "darkgreen"), lty=1, cex=0.8)
