README 

Replication file for "The Supply–Equity Trade-off: The Effect of Spatial Representation on the Local Housing Supply"

Authors: Michael Hankinson (GWU) and Asya Magazinnik (MIT)

Dataverse DOI: https://doi.org/10.7910/DVN/A4XYPS 

---

INCLUDED FILES

All files are described in greater detail in the sections below. 

Data: 
	1) housing_agg.csv
	2) housing_spatial.csv

Codebook: 
	1) codebook.pdf

Code:
	1) aggregate_outcomes.R
	2) pretrends.R
	3) pretrends_spatial.R
	4) spatial_outcomes.R
	5) table1.R

---

COMPUTING ENVIRONMENT

- All analyses were carried out on MacOS Monterey Version 12.2.1
- All R analyses were carried out with R version 4.1.3
- Additionally, R analyses use the following packages: 
	- ggplot2 (Version 3.3.5)
	- tidyverse (Version 1.3.1)
	- panelView (Version 1.1.9)
	- multiwayvcov (Version 1.2.3)
	- lmtest (Version 0.9-40)
	- stargazer (Version 5.2.3)
	- fect (Version 0.4.1)
	- bacondecomp (Version 0.1.1)
	- HMisc (Version 4.6-0)
	- clusterSEs (Version 2.6.5)

---

ANALYSIS DATASETS

1) housing_agg.csv
	The city by year data file that is used for our aggregate analysis.

2) housing_spatial.csv
	The block group by year data file that is used for our distributive (case study) analysis. 

---

CODE FOR REPRODUCING ALL RESULTS IN PAPER

1) aggregate_outcomes.R
	Conducts all analyses on the aggregate city-level housing data. Produces: 
	- Table 2
	- Appendix Figure A-1
	- Appendix Figure A-3
	- Appendix Figure A-4
	- Appendix Figure B-7
	- Appendix Figure B-8
	- Appendix Figure B-9
	- Appendix Table A-2
	- Appendix Table B-4
	- Appendix Table B-5
	- Appendix Table B-6
	- Appendix Table B-7	
	- Appendix Table B-8
	- Appendix Table B-9

2) pretrends.R
	Plots aggregate housing units permitted by treatment status and year relative to first district election. Produces:
	- Figure 1
	- Appendix Figure B-5

3) pretrends_spatial.R
	Plots housing units approved by treatment status and year relative to first district election for the case study sample. Produces:
	- Figure 3
	- Appendix Figure C-10 

4) spatial_outcomes.R
	Conducts all analyses on the distributive (case study) housing data. Produces: 
	- Figure 3
	- Table 3
	- Appendix Figure C-11
	- Appendix Figure C-12
	- Appendix Table A-3
	- Appendix Table C-10
	- Appendix Table C-11
	- Appendix Table C-12
	- Appendix Table C-13

5) table1.R
	Summarizes council representation by racial group. Produces: 
	- Table 1