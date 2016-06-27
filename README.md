# Anubis
This repository contains the Anubis tool for automated analysis of SRM data.
More info and downloads can be found at http://quantitativeprotoemics.org/anubis 

## Downloading
The latest release can be found under 'releases'

## Compilation
You need Java and Maven to compile Anubis and its dependencies. For convenience you may run the python installation script, which will compile and install the dependencies using Maven. Obviously this also requires an installed Python interpreter to work.

After compilation the Anubis binary will be found under anubis/target. The Reference Creator binary will be found under anubis-ref-creator/target. For both cases, the *.free.jar files contain all required dependencies and can be moved around freely.

## Running Anubis

1) Open ReferenceFileCreator.jar by double-clicking the file. (You need Java (can be downloaded at java.com))

2) Upload your transition list in Thermo csv or TraML format. If successful your precursors will be listed in the interface.

3) Load your reference files (in mzML format) with reference runs for these transitions.

4) Save the result as a reference file.

5) Run Anubis by double-clicking Anubis.jar or run it from the command line. 

6) Select the reference file from step 4. Select all your mzML-files which you want to analyse. Give the name for the result file and run.
 
