# FinishNFI

R script to format data from the Finnish Forest Intentory to FUNDIV template. R package ```targets``` is needed to run the script. 

A data folder should also be created, with 4 csv files inside: 
- fin_nfi_sample.csv contains tree level data
- sgdd_nfi_sample.csv contains sgdd (sum of growing degree days) data for each plotcode
- wai_nfi_sample.csv contains wai (water aridity index) for each plotcode
- species.csv contains correspondance between species code and latin name

The script can be run by two different ways. Either by opening R directly and run ```targets::tar_make()```, either by running ```sbatch batch.sh``` directly on the Linux console. 

The script will format the data, and return several csv files (in the output directory), and figures (in the fig directory). 
