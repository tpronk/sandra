# SANDRA Analysis Frameworks (SAFs) 
A SAF organizes your data analysis in a standard structure

folder | description
------ | -----------
interim/ | Interim data (any data derived from original or from other interim data)
meta/ |  Documentation
original/ | Original data
scripts/ | Analysis scripts
Load SANDRA.R | Loads SANDRA and sets up a FileIO object for this framework
Install SANDRA.R | Installs a SAF for this folder (by installing the SANDRA library and setting up "Load SANDRA.R")

The FileIO object constructed via "Load SANDRA.R" provides a consistent way to read and write data:
* Data is assumed to be UTF8 encoded, in a tab-separated format with variable names on the first row.
* Data is assumed to be in original/ or interim/
* Data may be read from original/ and interim/, but only written to interim/

# SAFs available in this repo
folder | description
------ | -----------
template/ | Offers template analysis scripts for decoding and scoring JASMIN1, JASMIN2, and SPRIF1 data
unit_testing/ |  Unit tests of SANDRA scoring scripts
