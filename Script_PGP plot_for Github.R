# The Planarized Globe Plot

# By David H. Nguyen, PhD, www.TSG-Lab.org

# Part A. This script calculates: 
# 1. r, which is the magnitude of a 3D vector of which we know it's xyz coordinate form the origin 
# 2. theta, the angle between x-axis and the vectors projection onto x-y plane
# 3. gamma, the angle between the y-axis and the vector's projection onto the y-z plane

# Part B. This script then plots the data according to the Planarized Globe Plot 
   # See the PDF that accompanies this script. 

# NOTE! This script assumes that your (x,y,z) coordinate system has the following orientation:
   # If you looking at the 3D object from its front face, the positive x-axis is coming   
        # out of the screen at you. The positive y-axis extends to your right, and the negative 
        # y-axis extends to your left. The positive z-axis is extending up, and the negative 
        # z-axis is extending down. 
  #  This means that if you look at the 3D object from above it (the top view), 
        # then you will be looking at the x-y plane. The positive x-axis extends to the right,
        # and the positive y-axis extends up. 
  # Based on this orientation scheme, the below table defines what the +/- signs of the 
        # x, y, and z coordinates should be if a point falls within a certain sector. 
        # See the PDF that accompanies this script for a visual explanation of how
        # the sectors are numbered. 

#   Sector.Number  Sector.Location   x.val  y.val  z.val
#              2       Top Right       >0    >0    >0
#              4    Bottom Right       >0    >0    <0
#              1        Top Left       >0    <0    >0
#              3     Bottom Left       >0    <0    <0
#              6       Top Right       <0    >0    >0
#              8    Bottom Right       <0    >0    <0
#              5        Top Left       <0    <0    >0
#              7     Bottom Left       <0    <0    <0



##### HOW TO FORMAT YOUR DATA FILE TO USE WITH THIS SCRIPT #####

# See the sample file that comes with this script: "Sample Data for PGP.csv"       
# 1. The data file should have AT LEAST three columns. It should be a csv file. 
# 2. The first column on the left should be the x coordinate. The second column
     # should be the y coordinate. The third column should be the z coordinate.
# 3. The file must not have no missing entries. The entries should be numbers.
# 4. Each row should be the x, y, and z coordinates for one point. 
# 5. The column headers for the coordinates MUST be called "xcoord", "ycoord", 
     # and "zcoord" or this script won't work.
# 5. This script was written to identify one group in your data. If you have more than
     # one treatment group in your data, you will have to modify this code to know that. 


# End of ReadMe Info

###########################################
###########################################
###########################################




###########################################
# Required packages for this script
library(ggplot2)
library(dplyr)


# Load your file
df = read.csv("Sample Data for PGP.csv")

# Change the names of the 3 columns to these 3 words. This line assumes that 
# the columns, from left to right, are the x, y, and z coordinates.  
colnames(df) = c('xcoord', 'ycoord', 'zcoord')





############################################ 
############################################ 
############################################ 

##### STEP 1
##### CALCULATE THE MAGNITUDE OF THE 3D VECTOR

# This function calculates the magnitude of the a vector from its xyz coordinates
# It is the distance formulate for three-coordinate points.

calculate.magnitude = function(n1, n2, n3) {
  sum.sq = (n1)^2 + (n2)^2 + (n3)^2
  return(sqrt(sum.sq))
}

v.mag = mapply(calculate.magnitude, df$xcoord, df$ycoord, df$zcoord)

# Create a new data frame to hold desired results. Call it "the.container"
the.container = data.frame(df, v.mag)

############################################ 
############################################ 
############################################ 

##### STEP 2
##### SOLVE FOR THETA

# Note: The trigonometric functions in R use radians, not degrees, 
# e.g. 360 degrees is 2pi radians. Converting from and to radians:


# z = r*cos(theta)
### Therefore, theta = arccos(z/r)

find.theta = function(n1,n2) {
  return(acos(n1/n2))
}

theta.rads = mapply(find.theta, df$zcoord, v.mag)

# Add theta.rads as a column in "the.container"
the.container = cbind(the.container, theta.rads)




############################################ 
############################################ 
############################################ 

##### STEP 3
##### SOLVE FOR PHI BASED ON THE VALUE OF THETA ABOVE

# y = r sin(phi) sin(theta)
# therefore, phi = arcsin[y/(r*sin(theta))]


calc.phi = function (a1,a2,a3) {
  asin((a1)/((a2)*(sin(a3))))
}

phi.rads = mapply(calc.phi, df$ycoord, v.mag, theta.rads)

# add phi.rads as a new column to "the.container"
the.container = cbind(the.container, phi.rads)



############################################ 
############################################ 
############################################ 

##### STEP 4
##### SOLVE FOR GAMMA (ANGLE BETWEEN Y AXIS AND Z AXIS)

# theta = atan(zcoord/ycoord)

calc.gamma = function(n1,n2){
  gamma = atan(n1/n2)
}

gamma.rads = mapply(calc.gamma, df$zcoord, df$ycoord)

# Add gamma.rads as a column in "the.container"
the.container = cbind(the.container, gamma.rads)



############################################ 
############################################ 
############################################ 

##### STEP 5
##### CONVERT PHI FROM RADIANS INTO DEGREES

# this function converts radians to degrees

rad2deg = function(num) {
  return((180 * num) / pi)
}


# Iterate the above function called rad2deg down the column called theta.radians 
# in the data frame called the.container
phi.degs = sapply(the.container$phi.rads, rad2deg)

# bind the result as a new column to "the.container"
the.container = cbind(the.container, phi.degs)


############################################ 
############################################ 
############################################ 

##### STEP 6
##### CONVERT GAMMA FROM RADIANS INTO DEGREES

# This function that converts radians to degrees

rad2deg = function(num) {
  return((180 * num) / pi)
}


# Iterate the function across the column called gamma.rads in the data frame called df

gamma.degs = sapply(the.container$gamma.rads, rad2deg)


# bind the result as a new column the.container

the.container = cbind(the.container, gamma.degs)



############################################ 
############################################ 
############################################ 

##### STEP 7
##### Filter the rows in "the.container" into each of the 8 sectors of a sphere, AND
# convert the angles to their proper +/- value for each sector.


# The following codes named "sector.N" are for an object whose frontview's top right 
# quadrant is in +x, +y, and +z axes. 

#   Sector.Number  Sector.Location   x.val  y.val  z.val
#              2       Top Right       >0    >0    >0
#              4    Bottom Right       >0    >0    <0
#              1        Top Left       >0    <0    >0
#              3     Bottom Left       >0    <0    <0
#              6       Top Right       <0    >0    >0
#              8    Bottom Right       <0    >0    <0
#              5        Top Left       <0    <0    >0
#              7     Bottom Left       <0    <0    <0


# These two functions turn a negative number positive, or a positive number negative

turn.positive = function(N){
  K = abs(N)
}

turn.negative = function(N){
  K = abs(N)
  L = (-1)*(K)
}

# This function calculates the supplementary angle AND THEN MAKES IT NEGATIVE
neg.supplement.angle = function(N){
  K = 180-(abs(N))
  L = (K)*(-1)
}

# This function calculates the supplementary angle
pos.supplement.angle = function(N){
  K = 180-(abs(N))
}


# For sector.1, theta is must be <0 and gamma must be 
# a supplementary angle in the positive-z direction.
sector.1 = filter(the.container, xcoord>0, ycoord<0, zcoord>0)
sector.1

phi.degs.convert = sapply(sector.1$phi.degs, turn.negative)
sector.1 = cbind(sector.1, phi.degs.convert)

# should be -(the value), not supplementary (changed already)
gamma.degs.convert = sapply(sector.1$gamma.degs, turn.negative)
sector.1 = cbind(sector.1, gamma.degs.convert) 


# For sector.2, theta is must be >0 and gamma must be >0. 
sector.2 = filter(the.container, xcoord>0, ycoord>0, zcoord>0)

phi.degs.convert = sapply(sector.2$phi.degs, turn.positive)
sector.2 = cbind(sector.2, phi.degs.convert)

gamma.degs.convert = sapply(sector.2$gamma.degs, turn.positive)
sector.2 = cbind(sector.2, gamma.degs.convert) 


# For sector.3, theta is must be <0 and gamma must be 
# a supplementary angle in negative-z direction.
sector.3 = filter(the.container, xcoord>0, ycoord<0, zcoord<0)

phi.degs.convert = sapply(sector.3$phi.degs, neg.supplement.angle)
sector.3 = cbind(sector.3, phi.degs.convert)

gamma.degs.convert = sapply(sector.3$gamma.degs, turn.negative)
sector.3 = cbind(sector.3, gamma.degs.convert) 



# For sector.4, theta is must be >0 and gamma must be <0.
sector.4 = filter(the.container, xcoord>0, ycoord>0, zcoord<0)

phi.degs.convert = sapply(sector.4$phi.degs, pos.supplement.angle)
sector.4 = cbind(sector.4, phi.degs.convert)

gamma.degs.convert = sapply(sector.4$gamma.degs, turn.negative)
sector.4 = cbind(sector.4, gamma.degs.convert) 


# For sector.5, theta must be -(180-(abs(theta.degs))), and gamma must be 
# a supplementary angle in the positive-z direction.
sector.5 = filter(the.container, xcoord<0, ycoord<0, zcoord>0)

phi.degs.convert = sapply(sector.5$phi.degs, neg.supplement.angle)
sector.5 = cbind(sector.5, phi.degs.convert)

gamma.degs.convert = sapply(sector.5$gamma.degs, pos.supplement.angle)
sector.5 = cbind(sector.5, gamma.degs.convert) 


# For sector.6, theta must be 180-(abs(theta.degs)), and gamma must be >0.
sector.6 = filter(the.container, xcoord<0, ycoord>0, zcoord>0)

phi.degs.convert = sapply(sector.6$phi.degs, pos.supplement.angle)
sector.6 = cbind(sector.6, phi.degs.convert)

#gamma should be supplemental positive (changed it already)
gamma.degs.convert = sapply(sector.6$gamma.degs, pos.supplement.angle)
sector.6 = cbind(sector.6, gamma.degs.convert) 


# For sector.7, theta is -(180-(abs(theta.degs))), and gamma is 180-(abs(gamma.degs))
sector.7 = filter(the.container, xcoord<0, ycoord<0, zcoord<0)

phi.degs.convert = sapply(sector.7$phi.degs, turn.negative)
sector.7 = cbind(sector.7, phi.degs.convert)

gamma.degs.convert = sapply(sector.7$gamma.degs, neg.supplement.angle)
sector.7 = cbind(sector.7, gamma.degs.convert) 


# For sector.8, theta is 180-(abs(theta.degs)), and gamma is 180-(abs(gamma.degs))
sector.8 = filter(the.container, xcoord<0, ycoord>0, zcoord<0)

phi.degs.convert = sapply(sector.8$phi.degs, turn.positive)
sector.8 = cbind(sector.8, phi.degs.convert)

gamma.degs.convert = sapply(sector.8$gamma.degs, neg.supplement.angle)
sector.8 = cbind(sector.8, gamma.degs.convert) 


# Combine all 8 sectors into one data frame called "final.container"
final.container = rbind(sector.1, sector.2, sector.3, sector.4, sector.5, sector.6, sector.7, sector.8)


# This line saves a .csv file on your computer called "the PGP data.csv"
# The file contains the information in the object called "the.container",
# which contains all the calculations that are plotted.
write.csv(final.container, "the PGP data.csv")



############################################ 
############################################ 
############################################ 

##### STEP 7
##### PLOT THE GRAPH

# Plot theta.degs.convert, gamma.degs.convert, and v.mag as x, y, and z dimensions, respectively.

# These lines plots the "r" coordinates markers of varying sizes, but of the same color.
plot.markersize = ggplot(final.container, aes(x=theta.degs.convert, y=gamma.degs.convert)) + 
  geom_point(aes(size=v.mag), shape = 1) + xlim(-180,180) + ylim(-180,180) + 
  geom_hline(yintercept=90, linetype="dashed", color = "black") + 
  geom_hline(yintercept=-90, linetype="dashed", color = "black") + 
  geom_hline(yintercept=0, linetype="solid", color = "black", size = 1) + 
  geom_vline(xintercept=90, linetype="dashed", color = "black") + 
  geom_vline(xintercept=-90, linetype="dashed", color = "black") + 
  geom_vline(xintercept=0, linetype="solid", color = "black", size = 1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


# Note: v.mag is the distance of the point from the center of the object. It's also
  # the magnitude of the vector extending from the center of the object to each point. 
plot.markersize




