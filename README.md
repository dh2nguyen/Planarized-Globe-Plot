# This is v2 of the ReadMe info, dated March 6, 2019.
# Changes: I edited the script and the sample data (The file is called "PGP Calibration Sample Data.csv") in order to make the assumed orientation of the 3D object more clear. See the attached original image and the output PGP graph.

# Note: This readme info is best viewed in a plain text reader.


# This script calculates: 
# 1. r, which is the magnitude of a 3D vector of which we know it's xyz coordinate form the origin 
# 2. theta, the angle between x-axis and the vectors projection onto x-y plane
# 3. gamma, the angle between the y-axis and the vector's projection onto the y-z plane
# ***NOTE: This script assumes that your (x,y,z) coordinates have ALREADY been 
  # centered such that the origin is (0,0,0).***

# 4. This script creates a graph that you need to save onto your computer.
# 5. This script creates a .csv file called "the PGP data.csv"

# Sample data is included in this folder. The file is called "PGP Calibration Sample Data.csv"

       ##### HOW TO FORMAT YOUR DATA FILE TO USE WITH THIS SCRIPT #####

# See the sample file that comes with this script: "Sample Data for PGP.csv"       
# 1. The data file should have AT LEAST three columns. It should be a csv file. 
# 2. The file must not have any missing entries. The entries should be numbers.
# 3. Each row should be the x/y/z coordinates for one point. 
# 4. The column headers for the coordinates MUST be called "xcoord", "ycoord", 
      # and "zcoord" or this script won't work. Just to be safe, the order of the 
      # columns should be like this:
        # xcoord      ycoord      zcoord    color
        # -59.0333	  28.9758	    25.689	  Red
        # -49.2063	  -30.6372	  -27.29    Blue
       
      # Reminder: This script assumes that your (x,y,z) coordinates have ALREADY been 
        # centered such that the origin is (0,0,0).
        # You needed to have defined where the origin should be. This algorithm does NOT do that.
       
# 5. This script was written to identify two different groups in the data. 
       # The first group should be called "Blue" and the second "Red". 
       # You should create a FOURTH column (from left) named "color", which defines whether 
       # each row belongs to the Blue group or Red group. 
       
      

###### End of ReadMe Info
