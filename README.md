# Saiz *et al* (2016) *Nature Communications*

This is the README file for the saiz-et-al_2016 repository associated to the article by Saiz *et al* (2016) *Asynchronous fate decisions at the single cell level collectively ensure a consistent lineage composition in the mouse blastocyst* **Nature Communications**, LINK, DOI.

This repository contains the data extracted from all embryo images in the study, the R scripts used to clean, transform and analyze the data and the code to generate all plots in the article.
All original microscopy images and raw image segmentation files can be found in FIGSHARE LINK.

## Files included

Data tables:
- **FGF_all_pooled_trans.csv**: all segmentation data, COMPLETE
- **scaling_all.rds**: all data for scaling experiments shown in Figs 4 and S6, corrected and transformed.
- **Experimental_log.csv**: summary of the complete dataset (FGF_all_pooled_trans.csv) listing each experimental group, the number of embryos present and the average cell count for that group.
- **Regimes.csv**: summary of the treatment regimes used and the corresponding stage at collection, fixation and treatment length (corresponds to Supplementary Table 1 in the article).
- **FGFonCD1_exp_ref.csv**: experimental reference containing the metadata - including experimental date, imaging date, experimenter and regime.

Scripts:
- **open_newdata.R**: R script to load and tidy the corrected data.
- **tables.R**: R script to generate summary tables containing information shown in the article figures as well as summary tables used to create plots.
- **stage.R**: R script to assign stage to embryos based on their cell count.
- **identify.R**: R script to assign identity to cells using the thresholding approach.
- **explore.R**: R script to do data exploration. Plots fluorescence intensity levels for the requested channel over Z, for each lineage.
- **figures_*n*-*m*.R**: R scripts to generate all plots present in the corresponding figures in the published article.

## Data structure

All segmentation data is amalgamated under a single data frame in .csv format (FGF_all_pooled_trans.csv). The file is made up of the following variables:
- **Experiment**: a unique identifier for each experiment, with the following structure: date (in MMDDYY format) + Regime (in R*n* format) + identifier (if necessary - such as for two experiments with the same date and regime). Each **experiment** corresponds to a single litter of embryos.
- **Embryo_ID**: a unique identifier for each embryo, with the following structure: Experiment_ID (as above) + Embryo identifier (Experimental group initial + embryo number). Initials for experimental groups are as follow:
	* C: Control
	* F: FGF4
	* P: PD03 (MEKi)
	* A: AZD4547 (FGFRi)
	* S: SU5402 (FGFRi)
	* LM: reference littermate
- **Experimenter**: initials of individual collecting and processing the sample. NS: Nestor Saiz; KW: Kiah M Williams.
- **Regime**: culture regime, as defined in Regimes.csv
- **Treatment**: treatment condition. Abreviations defined as follows:
	* Control: culture media (KSOM) with or without 1ug/ml of Heparin
	* FGF4_1000: KSOM + 1ug/ml (1000ng/ml)  of rhFGF4 + 1ug/ml of Heparin
	* PD03_1: KSOM + 1uM PD0325901
	* AZD_1: KSOM + 1uM AZD4547
	* SU_10: KSOM + 10uM SU5402 (not included in analysis)
	* SU_20: KSOM + 20uM SU5402
- **Tt_length**: length of treatment in the indicated conditions.
- **Cellcount**: total cell number of the embryo.
- **Cell_ID**: identifier for each nucleus (proxy for the cell) in an embryo. Cell_IDs correspond to the number assigned by MINS to each segmented nucleus, which can be found in the *overlaid.tiff* file for the corresponding embryo, at the corresponding XYZ coordinates.  
**NOTE:** Cell_IDs that are non-integers were generated during correction for undersegmentation. Fluorescence intensities for these nuclei have been measured manually and are reliable. However, XYZ coordinates may correspond to the nearest neighbor (corresponding integer Cell_ID).
- **Identity**: lineage identity, manually assigned by the experimenter. It has the following levels:
	* TE: trophectoderm - outer cell, CDX2+ (if applicable)
	* EPI: epiblast (NANOG+)
	* PRE: primitive endoderm (GATA6+)
	* DP: double positive (GATA6+, NANOG+)
	* DN: double negative (GATA6-, NANOG-)
- **Size**: volume of the segmented nucleus.
- **X**: X coordinate for that nucleus.
- **Y**: Y coordinate for that nucleus.
- **Z**: Z coordinate for that nucleus.
- **CH*n*.Avg**: average fluorescence intensity per pixel for the segmented nucleus in channel *n* (1-5). Labels for each channel for each embryo are specified in FILENAME.
- **CH*n*.Sum**: sum of total fluorescence intensity for the segmented nucleus in channel *n*.
- **TE_ICM**: TE or ICM (non TE) identity for each cell, assigned based on **Identity** (ICM = non TE cells).
- **Stage**: embryonic stage based on total cell count (Cellcount) using stage.R. Bins are <32, 32-64, 64-90, 90-120, 120-150, >150 cells. Intervals include the lower number but not the upper number of the interval.
- **Markers**: indicate the antibody combination used for each embryo:
	* 'C2G6NG': CDX2, GATA6, NANOG (rabbit)
	* 'O4G4NG': OCT4, GATA4, NANOG (rat)
- **CH*n*.logCor**: fluorescence intensity for channel *n* after correcting the log value of CH*n*.Avg for Z-associated fluorescence decay using correct.fgf.R.
- **CH*n*.ebLogCor**: fluorescence intensity for channel *n* after correcting the log value of CH*n*.Avg, per embryo, using Empirical Bayes approach.
- **Identity.km**: identity assigned automatically after defining clusters for EPI, DP, PRE and DN populations using the k-means clustering approach.
- **Exp_date**: date the experiment was started (embryo collection).
- **Img_date**: date the embryos were imaged.
- **Identity.lin**: identity assigned automatically using a thresholding method for the data on linear scale (after inverting the logarithm).

## Usage

To create the plots from the article:

1. Download all files to a local directory - respect the file names.
2. Using RStudio, create a new project in that directory and open the Rproject file.
3. Open *open_newdata.R* and *tables.R* and source them or load them from the console using source(*filename*.R). This will load the data files and generate the tables.
4. Open the figure files and source them or load them from the console using source(*filename*.R).

To explore the data and re-analyze, use your favorite software and the data structure above as reference.
