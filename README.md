# This is v3 of the ReadMe info, dated March 25, 2019. 

# Note: This readme info is best viewed in a plain text reader.

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
       
