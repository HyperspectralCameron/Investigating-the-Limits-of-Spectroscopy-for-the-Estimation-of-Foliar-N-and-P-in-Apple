Investigating the Limits of Spectroscopy for the Estimation of Foliar N and P in Apple

Herein lies the raw data and code to reproduce the results presented in Cullinan, C.B.,  Scomparin, A.N., Janik, K., & Tagliavini, M. (2025). Investigating the limits of spectroscopy for the estimation of foliar N and P in apple. Submitted for publication to Computers and Electronics in Agriculture.

R_script.R reads and integrates spectral and lab nutrient concentrations as well as metadata, processes the data and builds and assesses models of this data. All file paths are in the script are relative to the location of the script, therefore the file structure should be preserved. The original analysis presented in the original research article was performed using functions contained in the R package _hsdar_ as done in R_script_hsdar.R. _hsdar_ has, however, since been removed from CRAN and has rgdal as a dependency, which cannot run on R 4.5. R_script.R provides the same analysis that does not rely _hsdar_

Extract the raw data files into the main folder (not into its own folder). In the main folder there should be a folder for
each of the SED files, Excel trees and Analysis Results. The script should then access these directories without any need to
respecify file locations. 
