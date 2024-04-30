library(glmnet)

data <- read.csv("/Users/julie/Downloads/Cars_Data.csv")
View(data)

# Create correlation matrix
y <-  data[,17]
x <-  as.matrix(data[,2:16])
cor_mat = cor(x)		



##### Principal Components Analysis #####

out1 <-  eigen(cor_mat)		# eigen decomposition of correlation matrix
va <-  out1$values			# eigenvalues
ve <-  out1$vectors			# eigenvector



##### savings the plot as a pdf file #####
plot(va, ylab = "Eigenvalues", xlab = "Component Nos")				# scree plot


ego <- va[va > 1]							# eigenvalues > 1
nn <- nrow(as.matrix(ego))					# number of factors to retain
nn

out2 <- ve[,1:nn]							# eigenvectors associated with the reatined factors

out3 <- ifelse(abs(out2) < 0.3, 0, out2)		# ignore small values < 0.3
out3

print(colnames(data))

rownames(out3) <- c("Attractive", "Quiet","Unreliable","Poorly.Built","Interesting","Sporty","Uncomfortable","Roomy","Easy.Service","Prestige","Common","Economical","Successful","AvantGarde","Poor.Value")
out3

# flip the eigenvectors b/c later we will see that regression betas are negative. We flip the eignevector so that the slopes become positive for naming the four benefits

out4 <- (-1)*out3			# assign names based on the rownames and signs of coefficients
out4

## Naming of Benefits Z1 and Z2 are based on the matrix of eigenvectors in out4. 
# The numbers in out4 are *correlations* between the attributes (in rows) and the benefits (in cols) 
# Positive value means attribute_i (in rows) is positively correlated with Benefit Z (in columns)
# Negative value means attribute_i (in rows) is negatively correlated with Benefit Z (in columns)
# Small value means attribute_i (in rows) is weakly correlated with Benefit Z (in columns)
# Large value means attribute_i (in rows) is strongly correlated with Benefit Z (in columns)
# Zero value means attribute_i (in rows) is not correlated with Benefit Z (in columns). 

# Principal Componenent "Scores" are the linear combination of original variables, where the weights come from the eigenvectors.
# Denoted by Z in the slides

z <- x %*% out4			# Component Scores; coordinates of the brands in the map

out5 <- lm(y ~ z)		# Preference Regression to estimate how benefits drive overall preferences = f(benefits)

s5 <- summary(out5) 

#  Flip the negative Z variables by multiplying by -1. 
s5$coefficients[4:5]*(-1)


##### Iso Preference Line and Ideal Vector ####			

## Consider factors z1 and z2 with positive slopes on preferences
## Let z2 be the y-axis and z1 as the x-axis	
## Plot (z2, z1) to get brands in factor space of benefit 1 and benefit 2 

# coordinates of brands in (z1, z2) space
Z1 <- z[,1]
Z2 <- z[,2]
z.out <- cbind(Z1, Z2)
print(row.names(data))

rownames(z.out) = c("Infinity","Ford","Audi","Toyota","Eagle","Honda","Saab","Pontiac","BMW","Mercury")

# Plot, add labels, and save the brand map 
plot(Z1, Z2, main = "Brands in Upscale and Clumsy space", xlab = "Benefit Upscale", ylab = "Benefit Upscale", col = "lightblue", pch = 19, cex = 2)		# Brand Map in Z1-Z2 space
text(z.out, labels = row.names(z.out), font = 2, cex = 0.5, pos = 1)						# labeling brands


# Slopes of iso-preference and ideal vector	
b1 <- as.vector(coef(out5)[2])
b2 <- as.vector(coef(out5)[3])
slope.iso.preference = - b1/b2						
slope.ideal.vector = b2/b1 						

# Angles of iso-preference and ideal vector	
angle.iso.preference <- atan(slope.iso.preference)*180/pi	
angle.ideal.vector <- atan(slope.ideal.vector)*180/pi


## Bootstrap
library(stats) # For lm and prcomp
library(base) # For basic operations

bb <- 1000
angle.ideal.vector.samples <- numeric(bb)

for(ii in 1:bb) {
  data.star <- data[sample(nrow(data), nrow(data), replace = TRUE),]
  ystar <- data.star[, ncol(data.star)]
  Xstar <- as.matrix(data.star[, 2:(ncol(data.star)-1)]) 
  
  Xstar_std <- scale(Xstar)
  pca_result_star <- prcomp(Xstar_std)
  xstar <- pca_result_star$x[, 1:2]
  
  out.star <- lm(ystar ~ xstar)
  
  b1.star <- coef(out.star)[2]
  b2.star <- coef(out.star)[3]
  slope.ideal.vector.star <- b2.star / b1.star
  
  angle.ideal.vector.star <- atan(slope.ideal.vector.star) * 180 / pi
  
  angle.ideal.vector.samples[ii] <- angle.ideal.vector.star
}

angle.ideal.vector.CI <- quantile(angle.ideal.vector.samples, probs = c(0.025, 0.975))

print(angle.ideal.vector.CI)



