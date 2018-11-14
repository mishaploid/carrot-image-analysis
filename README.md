# Carrot Image Analysis 
These scripts are for processing data from a carrot image analysis pipeline (available on CyVerse in the PhytoMorph Image Analysis Toolkit). For more detail, see Turner et al. (2018) An automated image analysis pipeline enables genetic studies of shoot and root morphology in carrot (_Daucus carota_ L.) doi: 10.3389/fpls.2018.01703

## Phenotypic variation, correlations, and repeatability

**01_combine_hand_measurements.R** - cleans up raw phenotypic data and combines data from a diallel cross and an F2 mapping population

**02_shootPCA.R** - reads in results for the shoot biomass profile (SBP, generated through CyVerse) and performs principal component analysis

**03_rootPCA.R** - reads in results for the root biomass profile (generated through CyVerse) and performs principal component analysis on raw data and after normalization for length and width

**04_merge_hand_image.R** - combines hand and image derived measurements into a single file 

**05_correlations.R** - plots correlations between hand and image-measured traits

**06_repeatability.R** - calculates repeatability within and across environments for all traits 

## QTL mapping
**qtl_mapping/create_input_file.R** - creates input file for R/qtl using a linkage map and phenotype file

**qtl_mapping/mqm_mapping.R** - runs multiple qtl mapping (MQM) for all traits

**qtl_mapping/permutations.R** - dummy script for running permutations (see osg_scripts)

**qtl_mapping/plot_qtl_results.R** - generate LOD plots for all traits

**osg_scripts/** - contains files and libraries necessary to run qtl mapping and permutation testing on the Open Science Grid (OSG)
