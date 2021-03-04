# Start by installing and loading 'devtools' package, which is needed to instal the 'rlpi' package from GitHub
install.packages("devtools")
library(devtools)

# Install from main ZSL repository online
install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)

# Load the 'rlpi' package
library(rlpi)

# Set the random seed so that the stochastic process is repeatable
set.seed(41)

# Define the total number of individuals
J <- 5000
# Define the totla numebr of species
S <- 40
# Define the time interval for the simulation
years <- 1970:2020

# Create a blank community matrix, which species as rows and years as columns
comm <- matrix(0,ncol=length(years), nrow=S)

# Randomly sample the the community from S species in the first year
comm.start <- sample(1:S,J,replace=T)

# Count the number individuals of each species, create and identity vector, and add to the blank community matrix 
xtab<- table(comm.start)
id.vect <- as.numeric(names(xtab))
comm[id.vect,1] <- as.vector(xtab)

# Run a loop for all the subsequent years
for (j in 2:length(years)){

  # Sample a new community, using probabilities from the previous year abundance
  new.comm <- sample(1:S, J, replace=T, prob=comm[,j-1])

  # Count the number individuals of each species, create and identity vector, and add to the blank community matrix 
  xtab<- table(new.comm)
  id.vect <- as.numeric(names(xtab))
  comm[id.vect,j] <- as.vector(xtab)
}

# To calcualte the Living Planet Index, it is necessary to format the data as required by the `rlpi` package
# Add an ID vector and a vector of species identities
ID <- 1:S
Species <- as.factor(1:S)

# Create a matrix of population data, with appropriate column names 
Pop <- cbind(ID,Species,comm)
colnames(Pop) <- (c("ID","Binomial",paste0("X",as.factor(years))))

# The `rlpi`package includes an index if assessing a subset of the data. Since we are using the whole dataset, we only need a vector that includes all the rows.
index_vector <- rep(TRUE, S)

#This creates an 'infile' for calcualting the LPI
# Note: you need a folder names 'LPI_files' in your working directory
infile <- create_infile(as.data.frame(Pop), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi")
lpi <- LPIMain(infile, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

############################################################################################
#
#     Figure 1
#
############################################################################################

# Define the figure that will be produced, with its dimensions, layout and margins
png(filename="Fig1.png",width=20,height=10,units="cm",res=300)
par(mai=c(0.65,0.72,0.05,0.12))
par(mfrow=c(1,2))


#####################
#                   #
#     Panel A       #
#                   #
#####################

# First, create a blank plot of population size over year.
plot(0,0, type="n", xlim=c(1970,2020), ylim=c(0,max(comm)),
 xlab="Year",las=1, ylab="Population size", 	cex.axis=1.0, cex.lab= 1.2, mgp=c(2.2,0.6,0))

# Label the panel
mtext("(a)",cex=1.5, side = 3, adj = 0.075, line = -2,font=2)
	
# Run a loop to add each population one by one
for (k in 1:dim(comm)[1]){
	lines(years,comm[k,], col=rgb(0,0,0,0.5))
}


#####################
#                   #
#     Panel B       #
#                   #
#####################

# First, create a blank plot of LPI over year.
plot(0,0,type="n",las=1,xlim=c(1970,2020),
     ylim=c(0,1.2),ylab="LPI (1970 = 1)",xlab="Year", 
     cex.axis=1.0, cex.lab= 1.2, mgp=c(2.2,0.6,0))
# Add baseline
abline(h=1,col="grey")

# Include error bars as polygons
polygon(c(seq(1970,2020),seq(2020,1970)), 
    c(lpi$CI_low,rev(lpi$CI_high)),col=rgb(0,0,0.5,0.3),border=NA)

# Add mean line
lines(c(1970:2020),lpi$LPI_final,col=rgb(1,1,1,1),lwd=2)
# Lable the panel
mtext("(b)",cex=1.5, side = 3, adj = 0.075, line = -2,font=2)
# Close and save plot device
dev.off()


###############################################
###############################################

############################################################################################
#
#     Figure 2
#
############################################################################################

# Import the extinction probability data from the `Data` folder.
VU <-  read.table("Data/VU.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
EN <-  read.table("Data/EN.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
CR <-  read.table("Data/CR.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Define the figure that will be produced, with its dimensions, layout and margins
png(filename="Fig2.png",width=21,height=20,units="cm",res=300)
par(mai=c(0.65,0.72,0.05,0.12))
par(mfrow=c(2,2))

# First, create a blank plot of Extinction probability over (log) abundance.

plot(0,0,type="n",las=1,xlim=c(1,400),log="x",
     ylim=c(0,1.1),ylab="Extinction probability",xlab="Abundance", 
     cex.axis=1.0, cex.lab= 1.3, mgp=c(2.4,0.6,0))
# Add baseline
mtext("(a)",cex=1.5, side = 3, adj = 0.075, line = -2,font=2)
legend("topright",
	pch=c(1,15,16,NA,NA,NA), lty=c(1,1,2,1,1,1),
	col=c("black","grey", "black", "yellow", "orange", "red"),
	c("Extinction 100 years","Extinction 20 years","Extinction 10 years",
		"VU threshold", "EN threshold", "CR theshold"), bty="n", cex=0.95)



points(VU$d,VU$VU, type="o", col="black", lty=1, cex=1)
points(EN$d,EN$EN, type="o", col="grey", pch=15, cex=1)
points(CR$d,CR$CR, type="o", col="black", pch=16, lty=2,cex=1)


lines(c(1e-99,CR$d[max(which(CR$CR>=0.5))]),c(0.5,0.5), col="red")
lines(rep(CR$d[max(which(CR$CR>=0.5))],2),
	c(-1,CR$CR[max(which(CR$CR>=0.5))]), col="red",lty=2)

lines(c(1e-99,EN$d[max(which(EN$EN>=0.2))]),c(0.2,0.2), col="orange")
lines(rep(EN$d[max(which(EN$EN>=0.2))],2),
	c(-1,EN$EN[max(which(EN$EN>=0.2))]), col="orange",lty=2)

lines(c(1e-99,VU$d[max(which(VU$VU>=0.1))]),c(0.1,0.1), col="yellow")
lines(rep(VU$d[max(which(VU$VU>=0.1))],2),
	c(-1,VU$VU[max(which(VU$VU>=0.1))]), col="yellow",lty=2)


CR.t <- CR$d[max(which(CR$CR>=0.5))]
EN.t <- EN$d[max(which(EN$EN>=0.2))]
VU.t <- VU$d[max(which(VU$VU>=0.1))]

plot(0,0, type="n", xlim=c(1970,2020), ylim=c(0,max(comm)),
 xlab="Year",las=1, ylab="Population size",
	cex.axis=1.0, cex.lab= 1.3, mgp=c(2.4,0.6,0))
	

polygon(c(1960,1960,2300,2300), c(0,CR.t,CR.t,0),col=rgb(1,0,0,0.25),border=NA)	
polygon(c(1960,1960,2300,2300), c(CR.t,EN.t,EN.t,CR.t),col=rgb(1,0.3,0,0.1),border=NA)	
polygon(c(1960,1960,2300,2300), c(EN.t,VU.t,VU.t,EN.t),col=rgb(1,1,0,0.1),border=NA)	
polygon(c(1960,1960,2300,2300), c(VU.t,350,350,VU.t),col=rgb(0,0.6,0,0.1),border=NA)	



for (k in 1:dim(comm)[1]){
	lines(years,comm[k,], col=rgb(0,0,0,0.5))
}

mtext("(b)",cex=1.5, side = 3, adj = 0.075, line = -2,font=2)




rli.mat <- ifelse(comm>VU.t,1,ifelse(comm>EN.t,0.6,ifelse(comm>CR.t,0.4,ifelse(comm>0,0.2,1e-99))))

rli <- apply(rli.mat,2,mean)
rli.conf <- apply(rli.mat,2,function(x){sd(x)/sqrt(length(x))})


prop.mat <- matrix(0,ncol=length(years), nrow=5)
colnames(prop.mat) <- years; rownames(prop.mat) <- c("LC", "VU","EN", "CR", "EX")

for (j in 1:dim(rli.mat)[2]){

prop.mat[1,j] <- length(which(rli.mat[,j]==1))/length(rli.mat[,j]) *100
prop.mat[2,j] <- length(which(rli.mat[,j]==0.6))/length(rli.mat[,j]) *100
prop.mat[3,j] <- length(which(rli.mat[,j]==0.4))/length(rli.mat[,j]) *100
prop.mat[4,j] <- length(which(rli.mat[,j]==0.2))/length(rli.mat[,j]) *100
prop.mat[5,j] <- length(which(rli.mat[,j]<0.2))/length(rli.mat[,j]) *100
}


iucn.col <- c("green", "yellow","orange", "red", "black")
cum.mat <- apply(prop.mat,2,cumsum)




stepX <- function(x){
	x2 <- c(x[1],rep(x,each=2)[-1],1+x[length(x)], 1+x[length(x)])
	x2
}

stepY <- function(y){
	y2 <- c(0,rep(y, each=2),0)
	y2
}

plot(years,rli, type="n", xlim=c(1970,2020), ylim=c(0,110),
 xlab="Year",las=1, ylab="Pecentage of species (%)",
	cex.axis=1.0, cex.lab= 1.3, mgp=c(2.4,0.6,0)) 

polygon(c(years,max(years+1),max(years)+1,rev(years)),c(rep(100,1+length(years)),rep(0,1+length(years))),col="black", border=NA)
polygon(stepX(years),stepY(cum.mat[4,]),col="red", border="grey")
polygon(stepX(years),stepY(cum.mat[3,]),col="orange", border="grey")
polygon(stepX(years),stepY(cum.mat[2,]),col="yellow", border="grey")
polygon(stepX(years),stepY(cum.mat[1,]),col="green", border="grey")

for(i in 1:length(years)){
	lines(c(years[i],years[i]),c(0,100), col="grey")
}

legend(1975,15, bg="white",pch=15, col=iucn.col, c("LC","VU","EN","CR", "EX"),horiz=T)
mtext("(c)",cex=1.5, side = 3, adj = 0.075, line = -2,font=2)






plot(years,rli, type="n", xlim=c(1970,2020), ylim=c(0,1.1),
 xlab="Year",las=1, ylab="Red List Index",
	cex.axis=1.0, cex.lab= 1.3, mgp=c(2.4,0.6,0))

polygon(c(years,rev(years)), c(rli+rli.conf,rev(rli-rli.conf)),col=rgb(0.4,0,0,0.3),border=NA)	

lines(years,rli,col="white")

mtext("(d)",cex=1.5, side = 3, adj = 0.075, line = -2,font=2)

dev.off()


