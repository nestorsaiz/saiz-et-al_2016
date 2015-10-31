# Saiz *et al* (2016) JOURNAL-NAME

This is the README file for the REPO-NAME repository associated to the article by Saiz *et al* (2016) JOURNAL-NAME, LINK, DOI.

This repository contains the data extracted from all embryo images in the study, the R scripts used to clean, transform and analyze the data and the code to generate all plots in the article.
All original microscopy images and raw image segmentation files can be found in FIGSHARE LINK.

## Data structure

All segmentation data is amalgamated under a single data frame in .csv format. The file is made up of the following vectors:
- **Experiment**: a unique identifier for each experiment, with the following structure: date (in MMDDYY format) + Regime (in R*n* format) + identifier (if necessary).
- **Embryo_ID**: a unique identifier for each embryo, with the following structure: Experiment + Embryo identifier (Experimental group + number). Initials for experimental groups are as follow:
* C: Control
* F: FGF4
* P: PD03
* A: AZD4547
* S: SU5402
* LM: reference littermate
- **Experimenter**: initials of individual collecting and processing the sample. NS: Nestor Saiz; KW: Kiah Williams.
- **Regime**: culture regime, as define in Regimes.csv
- **Treatment**: treatment condition. Abreviations defined as follows:
* Control: culture media (KSOM) with or without 1ug/ml of Heparin
* FGF4_1000: KSOM + 1ug/ml (1000ng/ml)  of rhFGF4 + 1ug/ml of Heparin
* PD03_1: KSOM + 1uM PD0325901
* AZD_1: KSOM + 1uM AZD4547
* SU_10: KSOM + 10uM SU5402
* SU_20: KSOM + 20uM SU5402
- **Tt_length**: length of treatment in the indicated conditions.
- **Cellcount**: total cell number of the embryo.
- **Cell_ID**: identifier for each nucleus (proxy for the cell) in an embryo. Cell_IDs correspond to the number assigned by MINS to each segmented nucleus, which can be found in the *overlaid.tiff file for the corresponding embryo, on the corresponding XYZ coordinates.
NOTE: Cell_IDs that are non-integers were generated during correction for undersegmentation. Fluorescence intensities for these nuclei have been measured manually and are reliable. However, XYZ coordinates normally correspond to the nearest neighbor(corresponding integer Cell_ID).
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
- **CH*n*.Avg**: average fluorescence intensity per pixel for the segmented nucleus in channel *n*. Labels for each channel for each embryo are specified in Experimental_log.csv.
- **CH*n*.Sum**: summatory of total fluorescence intensity for the segmented nucleus in channel *n*.
