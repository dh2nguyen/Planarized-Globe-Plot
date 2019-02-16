# Planarized-Globe-Plot
The R Script for plotting 3D data -- (x,y,z) coordinates -- in the form of a Planarized Globe Plot (PGP) Transform

# The Planarized Globe Plot

# By David H. Nguyen, PhD, www.TSG-Lab.org

# Note: This file is best viewed in a plain text reader.

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

