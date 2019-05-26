# By David H. Nguyen, PhD, www.TSG-Lab.org

# ReadMe Info - Begin

# For a video tutorial explaining PGP see "Part 2" (starting at the 7:20 timepoint)
# in https://youtu.be/KbmhxKWt5Gg 


# This script calculates: 
# 1. r, which is the magnitude of a 3D vector of which we know it's xyz coordinate form the origin 
# 2. theta, the angle between x-axis and the vectors projection onto x-y plane
# 3. gamma, the angle between the y-axis and the vector's projection onto the y-z plane
# ***NOTE: This script assumes that your (x,y,z) coordinates have ALREADY been 
#   centered such that the origin is (0,0,0). You have to calculate the xyz coordinates
#   before you input the data in this script.***

# 4. This script will create a graph image that has 32 squares (Step 10). See the PDF in the 
#     GitHub folder for an explanation of how the squares are numbered. 
# 5. This script will create a first .csv file called "the PGP_32 data.csv" (Step 8).
# 6. This script will create a second .csv file called "PGP_32 for PTM.csv" (Step 9). This 
#       file is the input file for the script called "Calculate PTM for PGP" on GitHub.


       ##### HOW TO FORMAT YOUR DATA FILE TO USE WITH THIS SCRIPT #####

# This GitHub folder contains sample data called "PGP Calibration Sample Data.csv".
  # This folder also contains what the resulting plot from this data should look like.       
  # This folder also contains the original 3D image from which the sample data was extracted. 
         
              
# See the sample file that comes with this script: "PGP Calibration Sample Data.csv"       
# 1. The data file should ONLY have three columns. It should be a csv file. 
# 2. The file must not have any missing entries. 
# 3. The entries in the columns should be numbers.
# 4. Each row should be the x/y/z coordinates (from left column to right column) for one point. 
# 5. The column headers for the coordinates MUST be called "xcoord", "ycoord", 
      # and "zcoord" or this script won't work. Just to be safe, the order of the 
      # columns should be like this:
       
        # xcoord      ycoord      zcoord    
        # -59.0333    28.9758	    25.689	  
        # -49.206	    -30.6372	  -27.29    
       
# REMINDER: This script assumes that your (x,y,z) coordinates have ALREADY been 
#   centered such that the origin is (0,0,0).
#   You needed to have defined where the origin should be. This algorithm does NOT do that.
       
# 6. This script only plots one data set, meaning you have to run it separately for each sample. 
       
       
# ReadMe Info - End       
       
       
###########################################
# Required packages for this script
library(ggplot2)
library(dplyr)
library(data.table)
       
       
df = read.csv("change this file name.csv")
colnames(df) = c('xcoord', 'ycoord', 'zcoord')

############################################ 
############################################ 
############################################ 

##### STEP 1
##### CALCULATE THE MAGNITUDE OF THE 3D VECTOR

# This function calculates the magnitude of the a vector from its xyz coordinates
# It is the distance formula for three-coordinate points.

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
# quadrant (which is Sector 2) is in +x, -y, and +z axes. 

#   Sector.Number  Sector.Location   x.val  y.val  z.val
#              2       Top Right       >0    <0    >0
#              4    Bottom Right       >0    <0    <0
#              1        Top Left       <0    <0    >0
#              3     Bottom Left       <0    <0    <0
#              6       Top Right       >0    >0    >0
#              8    Bottom Right       >0    >0    <0
#              5        Top Left       <0    >0    >0
#              7     Bottom Left       <0    >0    <0


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
sector.1 = filter(the.container, xcoord<0, ycoord<0, zcoord>0)

phi.degs.convert = sapply(sector.1$phi.degs, turn.negative)
sector.1 = cbind(sector.1, phi.degs.convert)

# should be -(the value), not supplementary (changed already)
gamma.degs.convert = sapply(sector.1$gamma.degs, turn.positive)
sector.1 = cbind(sector.1, gamma.degs.convert) 



###
# Filter sector.1 into four subsequent sectors: 1a, 1b, 1c, 1d

# Paramters for sector.1's subsectors: 1a (aka 1), 1b (aka 2), 1c (aka 3), 1d (aka 4):
#   1a. phi.degs.convert: -45 to -90, gamma.degs.convert: 45 to 90 
#   1b. phi.degs.convert:  0 to -45, gamma.degs.convert: 45 to 90 
#   1c. phi.degs.convert:  -45 to -90, gamma.degs.convert: 0 to 45 
#   1d. phi.degs.convert:  0 to -45, gamma.degs.convert: 0 to 45 


sector1a.aka.1 = filter(sector.1, phi.degs.convert < -45, phi.degs.convert >= -90, gamma.degs.convert > 45, gamma.degs.convert <= 90)
sector1b.aka.2 = filter(sector.1, phi.degs.convert <= 0, phi.degs.convert >= -45, gamma.degs.convert > 45, gamma.degs.convert <= 90)
sector1c.aka.3 = filter(sector.1, phi.degs.convert < -45, phi.degs.convert >= -90, gamma.degs.convert >= 0, gamma.degs.convert <= 45)
sector1d.aka.4 = filter(sector.1, phi.degs.convert <= 0, phi.degs.convert >= -45, gamma.degs.convert >= 0, gamma.degs.convert <= 45)


# Create rownames and add as the first column in each sectorMX.aka.N dataframe
sector.name = rep("sector1a.aka.1", times = dim(sector1a.aka.1)[1])
sector1a.aka.1 = cbind(sector.name, sector1a.aka.1)

sector.name = rep("sector1b.aka.2", times = dim(sector1b.aka.2)[1])
sector1b.aka.2 = cbind(sector.name, sector1b.aka.2)

sector.name = rep("sector1c.aka.3", times = dim(sector1c.aka.3)[1])
sector1c.aka.3 = cbind(sector.name, sector1c.aka.3)

sector.name = rep("sector1d.aka.4", times = dim(sector1d.aka.4)[1])
sector1d.aka.4 = cbind(sector.name, sector1d.aka.4)



###############

# For sector.2, theta is must be >0 and gamma must be >0. 
sector.2 = filter(the.container, xcoord>0, ycoord<0, zcoord>0)

phi.degs.convert = sapply(sector.2$phi.degs, turn.positive)
sector.2 = cbind(sector.2, phi.degs.convert)

gamma.degs.convert = sapply(sector.2$gamma.degs, turn.positive)
sector.2 = cbind(sector.2, gamma.degs.convert) 



# Filter sector.2 into four subsequent sectors: 2a (aka 5), 2b (aka 6), 2c (aka 7), 2d (aka 8)

# Paramters for sector.2's 4 subsectors: 2a (aka 5), 2b (aka 6), 2c (aka 7), 2d (aka 8):
#   2a. phi.degs.convert: 0 to 45, gamma.degs.convert:  45 to 90
#   2b. phi.degs.convert: 45 to 90 , gamma.degs.convert:  45 to 90
#   2c. phi.degs.convert: 0 to 45 , gamma.degs.convert:  0 to 45
#   2d. phi.degs.convert: 45 to 90 , gamma.degs.convert: 0 to 45  


sector2a.aka.5 = filter(sector.2, phi.degs.convert >= 0,  phi.degs.convert <= 45,  gamma.degs.convert > 45, gamma.degs.convert <= 90)
sector2b.aka.6 = filter(sector.2, phi.degs.convert > 45, phi.degs.convert <= 90,  gamma.degs.convert > 45, gamma.degs.convert <= 90)
sector2c.aka.7 = filter(sector.2, phi.degs.convert >= 0,  phi.degs.convert <= 45,  gamma.degs.convert >= 0,  gamma.degs.convert <= 45)
sector2d.aka.8 = filter(sector.2, phi.degs.convert > 45, phi.degs.convert <= 90,  gamma.degs.convert >= 0,  gamma.degs.convert <= 45)


# Create rownames and add as the first column in each sectorMX.aka.N dataframe
sector.name = rep("sector2a.aka.5", times = dim(sector2a.aka.5)[1])
sector2a.aka.5 = cbind(sector.name, sector2a.aka.5)

sector.name = rep("sector2b.aka.6", times = dim(sector2b.aka.6)[1])
sector2b.aka.6 = cbind(sector.name, sector2b.aka.6)

sector.name = rep("sector2c.aka.7", times = dim(sector2c.aka.7)[1])
sector2c.aka.7 = cbind(sector.name, sector2c.aka.7)


sector.name = rep("sector2d.aka.8", times = dim(sector2d.aka.8)[1])
sector2d.aka.8 = cbind(sector.name, sector2d.aka.8)



####


# For sector.3, theta is must be <0 and gamma must be 
# a supplementary angle in negative-z direction.
sector.3 = filter(the.container, xcoord<0, ycoord<0, zcoord<0)

phi.degs.convert = sapply(sector.3$phi.degs, turn.negative)
sector.3 = cbind(sector.3, phi.degs.convert)

gamma.degs.convert = sapply(sector.3$gamma.degs, turn.negative)
sector.3 = cbind(sector.3, gamma.degs.convert) 


# Filter sector.3 into four subsequent sectors: 3a (aka 9), 3b (aka 10), 3c (aka 11), 3d (aka 12)

# Paramters for sector.3's 4 subsectors: 3a (aka 9), 3b (aka 10), 3c (aka 11), 3d (aka 12):
#   3a. phi.degs.convert: -45 to -90 , gamma.degs.convert: 0 to -45
#   3b. phi.degs.convert:  0 to -45 ,  gamma.degs.convert: 0 to -45  
#   3c. phi.degs.convert: -45 to -90 , gamma.degs.convert: -45 to -90
#   3d. phi.degs.convert:  0 to -45,   gamma.degs.convert: -45 to -90 


sector3a.aka.9 =  filter(sector.3, phi.degs.convert < -45,  phi.degs.convert >= -90,  gamma.degs.convert <= 0, gamma.degs.convert >= -45)
sector3b.aka.10 = filter(sector.3, phi.degs.convert <= 0,    phi.degs.convert >= -45,  gamma.degs.convert <= 0, gamma.degs.convert >= -45)
sector3c.aka.11 = filter(sector.3, phi.degs.convert < -45,  phi.degs.convert >= -90,  gamma.degs.convert < -45, gamma.degs.convert >= -90)
sector3d.aka.12 = filter(sector.3, phi.degs.convert <= 0,    phi.degs.convert >= -45,  gamma.degs.convert < -45, gamma.degs.convert >= -90)


# Create rownames and add as the first column in each sectorMX.aka.N dataframe
sector.name = rep("sector3a.aka.9", times = dim(sector3a.aka.9)[1])
sector3a.aka.9 = cbind(sector.name, sector3a.aka.9)

sector.name = rep("sector3b.aka.10", times = dim(sector3b.aka.10)[1])
sector3b.aka.10 = cbind(sector.name, sector3b.aka.10)

sector.name = rep("sector3c.aka.11", times = dim(sector3c.aka.11)[1])
sector3c.aka.11 = cbind(sector.name, sector3c.aka.11)

sector.name = rep("sector3d.aka.12", times = dim(sector3d.aka.12)[1])
sector3d.aka.12 = cbind(sector.name, sector3d.aka.12)




####
# For sector.4, theta is must be >0 and gamma must be <0.
sector.4 = filter(the.container, xcoord>0, ycoord<0, zcoord<0)

phi.degs.convert = sapply(sector.4$phi.degs, turn.positive)
sector.4 = cbind(sector.4, phi.degs.convert)

gamma.degs.convert = sapply(sector.4$gamma.degs, turn.negative)
sector.4 = cbind(sector.4, gamma.degs.convert) 



# Filter sector.X into four subsequent sectors: Xa, Xb, Xc, Xd

# Parameters for sector.4's four subseCtors: 4a (aka 13), 4b (aka 14), 4c (aka 15), 4d (aka 16):
#   4a. phi.degs.convert: 0 to 45 ,  gamma.degs.convert: 0 to -45  
#   4b. phi.degs.convert: 45 to 90 , gamma.degs.convert: 0 to -45  
#   4c. phi.degs.convert: 0 to 45 ,  gamma.degs.convert: -45 to -90  
#   4d. phi.degs.convert: 45 to 90 , gamma.degs.convert: -45 to -90 


sector4a.aka.13 = filter(sector.4, phi.degs.convert >= 0,  phi.degs.convert <= 45,  gamma.degs.convert <= 0, gamma.degs.convert >= -45)
sector4b.aka.14 = filter(sector.4, phi.degs.convert > 45, phi.degs.convert <= 90,  gamma.degs.convert <= 0, gamma.degs.convert >= -45)
sector4c.aka.15 = filter(sector.4, phi.degs.convert >= 0,  phi.degs.convert <= 45,  gamma.degs.convert < -45, gamma.degs.convert >= -90)
sector4d.aka.16 = filter(sector.4, phi.degs.convert > 45, phi.degs.convert <= 90,  gamma.degs.convert < -45, gamma.degs.convert >= -90)

# Create rownames and add as the first column in each sectorMX.aka.N dataframe
sector.name = rep("sector4a.aka.13", times = dim(sector4a.aka.13)[1])
sector4a.aka.13 = cbind(sector.name, sector4a.aka.13)

sector.name = rep("sector4b.aka.14", times = dim(sector4b.aka.14)[1])
sector4b.aka.14 = cbind(sector.name, sector4b.aka.14)

sector.name = rep("sector4c.aka.15", times = dim(sector4c.aka.15)[1])
sector4c.aka.15 = cbind(sector.name, sector4c.aka.15)

sector.name = rep("sector4d.aka.16", times = dim(sector4d.aka.16)[1])
sector4d.aka.16 = cbind(sector.name, sector4d.aka.16)




####
# For sector.5, theta must be -(180-(abs(theta.degs))), and gamma must be 
# a supplementary angle in the positive-z direction.
sector.5 = filter(the.container, xcoord<0, ycoord>0, zcoord>0)

phi.degs.convert = sapply(sector.5$phi.degs, neg.supplement.angle)
sector.5 = cbind(sector.5, phi.degs.convert)

gamma.degs.convert = sapply(sector.5$gamma.degs, pos.supplement.angle)
sector.5 = cbind(sector.5, gamma.degs.convert) 



# Filter sector.5 into four subsectors: 5a (aka 17), 5b (aka 18), 5c (aka 19), 5d (aka 20)

# Param3ters for sector.5's 4 subsectors: 5a (aka 17), 5b (aka 18), 5c (aka 19), 5d (aka 20):
#   5a. phi.degs.convert: -135 to -180, gamma.degs.convert: 135 to 180 
#   5b. phi.degs.convert: -135 to -90 , gamma.degs.convert: 135 to 180 
#   5c. phi.degs.convert: -135 to -180, gamma.degs.convert: 90 to 135 
#   5d. phi.degs.convert: -135 to -90 , gamma.degs.convert: 90 to 135 

sector5a.aka.17 = filter(sector.5, phi.degs.convert < -135, phi.degs.convert >= -180, gamma.degs.convert > 135, gamma.degs.convert <= 180)
sector5b.aka.18 = filter(sector.5, phi.degs.convert >= -135, phi.degs.convert < -90,  gamma.degs.convert > 135, gamma.degs.convert <= 180)
sector5c.aka.19 = filter(sector.5, phi.degs.convert < -135, phi.degs.convert >= -180, gamma.degs.convert > 90, gamma.degs.convert <= 135)
sector5d.aka.20 = filter(sector.5, phi.degs.convert >= -135, phi.degs.convert < -90,  gamma.degs.convert > 90, gamma.degs.convert <= 135)


# Create rownames and add as the first column in each sectorMX.aka.N dataframe
sector.name = rep("sector5a.aka.17", times = dim(sector5a.aka.17)[1])
sector5a.aka.17 = cbind(sector.name, sector5a.aka.17)

sector.name = rep("sector5b.aka.18", times = dim(sector5b.aka.18)[1])
sector5b.aka.18 = cbind(sector.name, sector5b.aka.18)

sector.name = rep("sector5c.aka.19", times = dim(sector5c.aka.19)[1])
sector5c.aka.19 = cbind(sector.name, sector5c.aka.19)

sector.name = rep("sector5d.aka.20", times = dim(sector5d.aka.20)[1])
sector5d.aka.20 = cbind(sector.name, sector5d.aka.20)



####
# For sector.6, theta must be 180-(abs(theta.degs)), and gamma must be >0.
sector.6 = filter(the.container, xcoord>0, ycoord>0, zcoord>0)

phi.degs.convert = sapply(sector.6$phi.degs, pos.supplement.angle)
sector.6 = cbind(sector.6, phi.degs.convert)

#gamma should be supplemental positive (changed it already)
gamma.degs.convert = sapply(sector.6$gamma.degs, pos.supplement.angle)
sector.6 = cbind(sector.6, gamma.degs.convert) 


# Filter sector.X into four subsequent sectors: 6a (aka 21), 6b (aka 22), 6c (aka 23), 6d (aka 24)

# Parameters for sector.6's four subsectors: 6a (aka 21), 6b (aka 22), 6c (aka 23), 6d (aka 24):
#   6a. phi.degs.convert: 90 to 135, gamma.degs.convert: 135 to 180
#   6b. phi.degs.convert: 135 to 180 , gamma.degs.convert: 135 to 180  
#   6c. phi.degs.convert: 90 to 135 , gamma.degs.convert: 90 to 135 
#   6d. phi.degs.convert: 135 to 180 , gamma.degs.convert: 90 to 135  


sector6a.aka.21 = filter(sector.6, phi.degs.convert > 90,  phi.degs.convert <= 135,  gamma.degs.convert > 135, gamma.degs.convert <= 180)
sector6b.aka.22 = filter(sector.6, phi.degs.convert > 135, phi.degs.convert <= 180,  gamma.degs.convert > 135, gamma.degs.convert <= 180)
sector6c.aka.23 = filter(sector.6, phi.degs.convert > 90,  phi.degs.convert <= 135,  gamma.degs.convert > 90,  gamma.degs.convert <= 135)
sector6d.aka.24 = filter(sector.6, phi.degs.convert > 135, phi.degs.convert <= 180,  gamma.degs.convert > 90,  gamma.degs.convert <= 135)

# Create rownames and add as the first column in each sectorMX.aka.N dataframe
sector.name = rep("sector6a.aka.21", times = dim(sector6a.aka.21)[1])
sector6a.aka.21 = cbind(sector.name, sector6a.aka.21)

sector.name = rep("sector6b.aka.22", times = dim(sector6b.aka.22)[1])
sector6b.aka.22 = cbind(sector.name, sector6b.aka.22)

sector.name = rep("sector6c.aka.23", times = dim(sector6c.aka.23)[1])
sector6c.aka.23 = cbind(sector.name, sector6c.aka.23)

sector.name = rep("sector6d.aka.24", times = dim(sector6d.aka.24)[1])
sector6d.aka.24 = cbind(sector.name, sector6d.aka.24)


####
# For sector.7, theta is -(180-(abs(theta.degs))), and gamma is 180-(abs(gamma.degs))
sector.7 = filter(the.container, xcoord<0, ycoord>0, zcoord<0)

phi.degs.convert = sapply(sector.7$phi.degs, neg.supplement.angle)
sector.7 = cbind(sector.7, phi.degs.convert)

gamma.degs.convert = sapply(sector.7$gamma.degs, neg.supplement.angle)
sector.7 = cbind(sector.7, gamma.degs.convert) 


# Filter sector.X into four subsequent sectors: 7a (aka 25), 7b (aka 26), 7c (aka 27), 7d (aka 28)

# Parameters for sector.7's four subsectors: 7a (aka 25), 7b (aka 26), 7c (aka 27), 7d (aka 28):
#   7a. phi.degs.convert: -135 to -180, gamma.degs.convert: -90 to -135 
#   7b. phi.degs.convert: -90 to -135 , gamma.degs.convert: -90 to -135  
#   7c. phi.degs.convert: -135 to -180 , gamma.degs.convert: -135 to -180  
#   7d. phi.degs.convert: -90 to -135 , gamma.degs.convert: -135 to -180  


sector7a.aka.25 = filter(sector.7, phi.degs.convert < -135,  phi.degs.convert >= -180,  gamma.degs.convert < -90, gamma.degs.convert >= -135)
sector7b.aka.26 = filter(sector.7, phi.degs.convert < -90,    phi.degs.convert >= -135, gamma.degs.convert < -90, gamma.degs.convert >= -135)
sector7c.aka.27 = filter(sector.7, phi.degs.convert < -135,  phi.degs.convert >= -180,  gamma.degs.convert < -135, gamma.degs.convert >= -180)
sector7d.aka.28 = filter(sector.7, phi.degs.convert < -90,    phi.degs.convert >= -135, gamma.degs.convert < -135, gamma.degs.convert >= -180)


# Create rownames and add as the first column in each sectorMX.aka.N dataframe
sector.name = rep("sector7a.aka.25", times = dim(sector7a.aka.25)[1])
sector7a.aka.25 = cbind(sector.name, sector7a.aka.25)

sector.name = rep("sector7b.aka.26", times = dim(sector7b.aka.26)[1])
sector7b.aka.26 = cbind(sector.name, sector7b.aka.26)

sector.name = rep("sector7c.aka.27", times = dim(sector7c.aka.27)[1])
sector7c.aka.27 = cbind(sector.name, sector7c.aka.27)

sector.name = rep("sector7d.aka.28", times = dim(sector7d.aka.28)[1])
sector7d.aka.28 = cbind(sector.name, sector7d.aka.28)


####
# For sector.8, theta is 180-(abs(theta.degs)), and gamma is 180-(abs(gamma.degs))
sector.8 = filter(the.container, xcoord>0, ycoord>0, zcoord<0)

phi.degs.convert = sapply(sector.8$phi.degs, pos.supplement.angle)
sector.8 = cbind(sector.8, phi.degs.convert)

gamma.degs.convert = sapply(sector.8$gamma.degs, neg.supplement.angle)
sector.8 = cbind(sector.8, gamma.degs.convert) 



# Filter sector.8 into four subsectors: 8a (aka 29), 8b (aka 30), 8c (aka 31), 8d (aka 32)

# Parameters for sector.8's four subsectors: 8a (aka 29), 8b (aka 30), 8c (aka 31), 8d (aka 32):
#   8a. phi.degs.convert: 90 to 135, gamma.degs.convert: -90 to -135  
#   8b. phi.degs.convert: 135 to 180 , gamma.degs.convert: -90 to -135  
#   8c. phi.degs.convert: 90 to 135 , gamma.degs.convert: -135 to -180 
#   8d. phi.degs.convert: 135 to 180 , gamma.degs.convert: -135 to -180  


sector8a.aka.29 = filter(sector.8, phi.degs.convert > 90,  phi.degs.convert <= 135,  gamma.degs.convert < -90, gamma.degs.convert >= -135)
sector8b.aka.30 = filter(sector.8, phi.degs.convert > 135, phi.degs.convert <= 180,  gamma.degs.convert < -90, gamma.degs.convert >= -135)
sector8c.aka.31 = filter(sector.8, phi.degs.convert > 90,  phi.degs.convert <= 135,  gamma.degs.convert < -135, gamma.degs.convert >= -180)
sector8d.aka.32 = filter(sector.8, phi.degs.convert > 135, phi.degs.convert <= 180,  gamma.degs.convert < -135, gamma.degs.convert >= -180)

# Create rownames and add as the first column in each sectorMX.aka.N dataframe
sector.name = rep("sector8a.aka.29", times = dim(sector8a.aka.29)[1])
sector8a.aka.29 = cbind(sector.name, sector8a.aka.29)

sector.name = rep("sector8b.aka.30", times = dim(sector8b.aka.30)[1])
sector8b.aka.30 = cbind(sector.name, sector8b.aka.30)

sector.name = rep("sector8c.aka.31", times = dim(sector8c.aka.31)[1])
sector8c.aka.31 = cbind(sector.name, sector8c.aka.31)

sector.name = rep("sector8d.aka.32", times = dim(sector8d.aka.32)[1])
sector8d.aka.32 = cbind(sector.name, sector8d.aka.32)


#########
# Combine all 8 sectors into one data frame called "final.container"

final.container = rbind(sector1a.aka.1, sector1b.aka.2, sector1c.aka.3, sector1d.aka.4, 
    sector2a.aka.5, sector2b.aka.6, sector2c.aka.7, sector2d.aka.8, 
    sector3a.aka.9, sector3b.aka.10, sector3c.aka.11, sector3d.aka.12, 
    sector4a.aka.13, sector4b.aka.14, sector4c.aka.15, sector4d.aka.16, 
    sector5a.aka.17, sector5b.aka.18, sector5c.aka.19, sector5d.aka.20, 
    sector6a.aka.21, sector6b.aka.22, sector6c.aka.23, sector6d.aka.24, 
    sector7a.aka.25, sector7b.aka.26, sector7c.aka.27, sector7d.aka.28, 
    sector8a.aka.29, sector8b.aka.30, sector8c.aka.31, sector8d.aka.32)





############################################ 
############################################ 
############################################ 

##### STEP 8
##### Combine all sectors together. For sectors that have no rows and are excluded 
#####   from final.container, they need to be added to final.container as a row of zeros. 


####
# Find all unique sectors that are in final.container$sector.name
unique.present.sectors = unique(final.container$sector.name)
# Turn this into a vector
unique.present.sectors = as.vector(unique(final.container$sector.name))

# Create a vector of all possible sector names for comparision with unique.present.sectors
all.sectors = c("sector1a.aka.1", "sector1b.aka.2", "sector1c.aka.3", "sector1d.aka.4", 
                "sector2a.aka.5", "sector2b.aka.6", "sector2c.aka.7", "sector2d.aka.8", 
                "sector3a.aka.9", "sector3b.aka.10", "sector3c.aka.11", "sector3d.aka.12", 
                "sector4a.aka.13", "sector4b.aka.14", "sector4c.aka.15", "sector4d.aka.16", 
                "sector5a.aka.17", "sector5b.aka.18", "sector5c.aka.19", "sector5d.aka.20", 
                "sector6a.aka.21", "sector6b.aka.22", "sector6c.aka.23", "sector6d.aka.24", 
                "sector7a.aka.25", "sector7b.aka.26", "sector7c.aka.27", "sector7d.aka.28", 
                "sector8a.aka.29", "sector8b.aka.30", "sector8c.aka.31", "sector8d.aka.32")

# Find the sectors that are NOT present in unique.present.sectors
absent.sectors = setdiff(all.sectors, unique.present.sectors)
absent.sectors = as.data.frame(absent.sectors)

zero.vec = rep(0, times = (dim(final.container)[2]-1))
zero.vec.multi = rep(zero.vec, times = dim(absent.sectors)[1])
zero.vec.df = matrix(zero.vec.multi, nrow = dim(absent.sectors)[1], ncol = (dim(final.container)[2]-1))

columnNames = colnames(final.container)

rows2add = cbind(absent.sectors, zero.vec.df)

colNames.rows2add = colnames(rows2add)

rows2add = setnames(rows2add, old = c(colNames.rows2add), new = c(columnNames))


#############

# Combine rows2add to final.container for exporting
export.me = rbind(final.container, rows2add)


# This line saves a .csv file on your computer called "the PGP data.csv"
  # The file contains the information in the object called "the.container",
  # which contains all the calculations that are plotted.
write.csv(export.me, "the PGP_32 data.csv", row.names = F)



############################################ 
############################################ 
############################################ 

##### STEP 9
##### Prepare PGP data for Pavlidis Template Matching (PTM) Analysis


# Subset out rows by sector names in column called sector.name
subsec_1 = filter(export.me, grepl('sector1a.aka.1', sector.name))
subsec_2 = filter(export.me, grepl('sector1b.aka.2', sector.name))
subsec_3 = filter(export.me, grepl('sector1c.aka.3', sector.name))
subsec_4 = filter(export.me, grepl('sector1d.aka.4', sector.name))
subsec_5 = filter(export.me, grepl('sector2a.aka.5', sector.name))
subsec_6 = filter(export.me, grepl('sector2b.aka.6', sector.name))
subsec_7 = filter(export.me, grepl('sector2c.aka.7', sector.name))
subsec_8 = filter(export.me, grepl('sector2d.aka.8', sector.name))
subsec_9 = filter(export.me, grepl('sector3a.aka.9', sector.name))
subsec_10 = filter(export.me, grepl('sector3b.aka.10', sector.name))
subsec_11 = filter(export.me, grepl('sector3c.aka.11', sector.name))
subsec_12 = filter(export.me, grepl('sector3d.aka.12', sector.name))
subsec_13 = filter(export.me, grepl('sector4a.aka.13', sector.name))
subsec_14 = filter(export.me, grepl('sector4b.aka.14', sector.name))
subsec_15 = filter(export.me, grepl('sector4c.aka.15', sector.name))
subsec_16 = filter(export.me, grepl('sector4d.aka.16', sector.name))
subsec_17 = filter(export.me, grepl('sector5a.aka.17', sector.name))
subsec_18 = filter(export.me, grepl('sector5b.aka.18', sector.name))
subsec_19 = filter(export.me, grepl('sector5c.aka.19', sector.name))
subsec_20 = filter(export.me, grepl('sector5d.aka.20', sector.name))
subsec_21 = filter(export.me, grepl('sector6a.aka.21', sector.name))
subsec_22 = filter(export.me, grepl('sector6b.aka.22', sector.name))
subsec_23 = filter(export.me, grepl('sector6c.aka.23', sector.name))
subsec_24 = filter(export.me, grepl('sector6d.aka.24', sector.name))
subsec_25 = filter(export.me, grepl('sector7a.aka.25', sector.name))
subsec_26 = filter(export.me, grepl('sector7b.aka.26', sector.name))
subsec_27 = filter(export.me, grepl('sector7c.aka.27', sector.name))
subsec_28 = filter(export.me, grepl('sector7d.aka.28', sector.name))
subsec_29 = filter(export.me, grepl('sector8a.aka.29', sector.name))
subsec_30 = filter(export.me, grepl('sector8b.aka.30', sector.name))
subsec_31 = filter(export.me, grepl('sector8c.aka.31', sector.name))
subsec_32 = filter(export.me, grepl('sector8d.aka.32', sector.name))



# For each sector, calculate the median for the following three columns: 
#   phi.degs.convert, gamma.degs.convert, and v.mag


# the following functions print recurring commands
phi.fxn = function (i) {
  paste0("subsec_", i, ".phi = summarise(subsec_", i, ", mean(phi.degs.convert))")
}
  
gam.fxn = function (i) {
  paste0("subsec_", i, ".gamma = summarise(subsec_", i, ", mean(gamma.degs.convert))")
}

vmag.fxn = function (i) {
  paste0("subsec_", i, ".vmag = summarise(subsec_", i, ", mean(v.mag))")
}


####
how.many.sectors = 32
iter.guide = seq(1: how.many.sectors)

# The following functions apply commands to items in iter.guide
all.phi = sapply(iter.guide, phi.fxn)
all.gamma = sapply(iter.guide, gam.fxn)
all.vmag = sapply(iter.guide, vmag.fxn)

# Create for loop that parses the command lines

for (i in all.phi){
  eval(parse(text=i))
}

for (i in all.gamma){
  eval(parse(text=i))
}

for (i in all.vmag){
  eval(parse(text=i))
}



#####

# Bind results into a single vector that has the following order of items for each sector: 
#   1) phi.degs.convert 
#   2) gamma.degs.convert 
#   3) v.mag
#



bind.all.3 = function (i){
   paste0("concat_sec_",i," = c(subsec_", i,".phi, subsec_", i,".gamma, subsec_",i,".vmag)")
 }

make.command = sapply(iter.guide, bind.all.3)

for (i in make.command){
  eval(parse(text=i))
}


prefix = rep("concat_sec_", times = how.many.sectors)
suffix = seq(1:how.many.sectors)
names_concat = paste0(prefix, suffix)
names_concat_copy = names_concat

funky1 = function(i1, i2){
  paste0(i1," = data.frame(",i2,")")
}

convert.2.df = mapply(funky1, names_concat, names_concat_copy)

for (i in convert.2.df){
  eval(parse(text=i))
}


# bind all concat_sec_X into a vector
horiz.df = cbind(concat_sec_1, concat_sec_2, concat_sec_3, concat_sec_4, 
                 concat_sec_5, concat_sec_6, concat_sec_7, concat_sec_8,
                 concat_sec_9, concat_sec_10, concat_sec_11, concat_sec_12, 
                 concat_sec_13, concat_sec_14, concat_sec_15,concat_sec_16, 
                 concat_sec_17, concat_sec_18, concat_sec_19, concat_sec_20,
                 concat_sec_21, concat_sec_22, concat_sec_23, concat_sec_24, 
                 concat_sec_25, concat_sec_26, concat_sec_27, concat_sec_28, 
                 concat_sec_29, concat_sec_30, concat_sec_31, concat_sec_32)


vert.df = t(horiz.df) # Transpose the vector into a vertical object
vert.df = data.frame(vert.df) # Turn the vector into a data frame

# Create a vector that has triplicates of values from 1:32
triple = rep(1:32, each = 3)

# Add a column that names which sector each row belongs to
vert.df = cbind(triple, vert.df)

#########
name1 = rep("angle.phi", times = how.many.sectors)
name2 = rep("angle.gamma", times = how.many.sectors)
name3 = rep("magnitude", times = how.many.sectors)

name.df = rbind(name1, name2, name3)

#########
funky3 = function(i){
  paste0("item_",i," = as.list(name.df[,", i, "])")
}

print.commands = sapply(iter.guide, funky3)

for (i in print.commands){
  eval(parse(text=i))
}


#########
triple.names = list()

funky4 = function(i){
  paste0("triple.names = append(triple.names, item_", i,")")
}

print.triple = sapply(iter.guide, funky4)

for (i in print.triple){
  eval(parse(text=i))
}

triple.names = t(data.frame(triple.names))
#########

vert.df = cbind(triple.names, vert.df)


# Rename the two columns in vert.df
colnames(vert.df) = c("coordinate", "sector_name", "mean_value")


#####

# Create a .csv file 
write.csv(vert.df, "PGP_32 for PTM.csv", row.names = F)



############################################ 
############################################ 
############################################ 

##### STEP 10
##### PLOT THE GRAPH

# Plot theta.degs, gamma.degs, and v.mag as x, y, and z dimensions, respectively.

# These lines plots the "r" coordinates markers of varying sizes, but of the same color.
plot.magnit.as.color = ggplot(final.container, aes(x=phi.degs.convert, y=gamma.degs.convert)) + 
  geom_point(aes(color=v.mag), shape = 16, size = 8) + 
  scale_color_gradient(low = "green", high = "purple") +
  geom_hline(yintercept=45, linetype="dashed", color = "black") + 
  geom_hline(yintercept=90, linetype="dashed", color = "black") + 
  geom_hline(yintercept=135, linetype="dashed", color = "black") + 
  geom_hline(yintercept=180, linetype="dashed", color = "black") + 
  geom_hline(yintercept=-180, linetype="dashed", color = "black") + 
  geom_hline(yintercept=-135, linetype="dashed", color = "black") + 
  geom_hline(yintercept=-90, linetype="dashed", color = "black") + 
  geom_hline(yintercept=-45, linetype="dashed", color = "black") + 
  geom_hline(yintercept=0, linetype="solid", color = "black", size = 1) + 
  geom_vline(xintercept=45, linetype="dashed", color = "black") + 
  geom_vline(xintercept=90, linetype="dashed", color = "black") + 
  geom_vline(xintercept=135, linetype="dashed", color = "black") + 
  geom_vline(xintercept=180, linetype="dashed", color = "black") + 
  geom_vline(xintercept=-45, linetype="dashed", color = "black") + 
  geom_vline(xintercept=-90, linetype="dashed", color = "black") + 
  geom_vline(xintercept=-135, linetype="dashed", color = "black") + 
  geom_vline(xintercept=-180, linetype="dashed", color = "black") + 
  geom_vline(xintercept=0, linetype="solid", color = "black", size = 1) +
  annotate("rect", xmin = -90, xmax = 90, ymin = -90, ymax = 90, alpha = .2) +
  annotate("rect", xmin = -180, xmax = -90, ymin = 90, ymax = 180, alpha = .2) +
  annotate("rect", xmin = -180, xmax = -90, ymin = -90, ymax = -180, alpha = .2) +
  annotate("rect", xmin = 90, xmax = 180, ymin = -90, ymax = -180, alpha = .2) +
  annotate("rect", xmin = 90, xmax = 180, ymin = 90, ymax = 180, alpha = .2) +
  scale_y_continuous(breaks = seq(-180, 180, 45)) + 
  scale_x_continuous(breaks = seq(-180, 180, 45)) + 
  xlab("The Angle Phi (degrees)") + ylab("The Angle Gamma (degrees)") +
  labs(col="Distance \n From \n Origin") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  

plot.magnit.as.color




