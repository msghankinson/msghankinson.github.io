CONSTRUCTION OF THE ANALYSIS DATASETS

There are two datasets analyzed in the paper: 

(1) Aggregate housing data (housing_agg.csv)
	
	This dataset is constructed from the authors' original data collection on electoral institutions across California cities over time, along with aggregate housing permit data and controls from the Census. 

	Specifically, it is comprised of the following inputs: 

	I. DEPENDENT VARIABLES

		(1) Housing permits by city 
			Downloaded from: https://socds.huduser.gov/permits/
			Stored in: raw/aggregatePermits/
			File(s): 
			- statewide_190715.csv
			- statewide_2019.csv

		(2) Housing permits by city and affordability status 
			Downloaded from: https://www.hcd.ca.gov/apr-data-dashboard-and-downloads and personal communication with HCD staff via apr@hcd.ca.gov
			Stored in: raw/aggregatePermits/
			File(s): 
			- affordability_mh.xlsx
			- affordability_2020.xlsb

	II. INDEPENDENT VARIABLES

		(1) Authors' original data collection
			Stored in: raw/
			File(s): cvraCities.csv

	III. COVARIATES

		(1) Population and race data (Census/ACS)
			Downloaded from: https://factfinder.census.gov
			Stored in: raw/census/race/cdp/
			File(s):
			- ACS_10_5YR_B03002_with_ann.csv
			- ACS_11_5YR_B03002_with_ann.csv
			- ACS_12_5YR_B03002_with_ann.csv
			- ACS_13_5YR_B03002_with_ann.csv
			- ACS_14_5YR_B03002_with_ann.csv
			- ACS_15_5YR_B03002_with_ann.csv
			- ACS_16_5YR_B03002_with_ann.csv
			- ACS_17_5YR_B03002_with_ann.csv
			- ACS_18_5YR_B03002_with_ann.csv

		(2) Income data (Census/ACS)
			Downloaded from: https://factfinder.census.gov
			Stored in: raw/census/income/cdp/
			File(s):
			- ACS_10_5YR_S1903_with_ann.csv
			- ACS_11_5YR_S1903_with_ann.csv
			- ACS_12_5YR_S1903_with_ann.csv
			- ACS_13_5YR_S1903_with_ann.csv
			- ACS_14_5YR_S1903_with_ann.csv
			- ACS_15_5YR_S1903_with_ann.csv
			- ACS_16_5YR_S1903_with_ann.csv
			- ACS_17_5YR_S1903_with_ann.csv
			- ACS_18_5YR_S1903_with_ann.csv

		(3) Occupancy data (Census/ACS)
			Downloaded from: https://factfinder.census.gov
			Stored in: raw/census/occupancy/cdp/
			File(s):
			- ACS_10_5YR_B25002/ACS_10_5YR_B25002_with_ann.csv
			- ACS_11_5YR_B25002/ACS_11_5YR_B25002_with_ann.csv
			- ACS_12_5YR_B25002/ACS_12_5YR_B25002_with_ann.csv
			- ACS_13_5YR_B25002/ACS_13_5YR_B25002_with_ann.csv
			- ACS_14_5YR_B25002/ACS_14_5YR_B25002_with_ann.csv
			- ACS_15_5YR_B25002/ACS_15_5YR_B25002_with_ann.csv
			- ACS_16_5YR_B25002/ACS_16_5YR_B25002_with_ann.csv
			- ACS_17_5YR_B25002/ACS_17_5YR_B25002_with_ann.csv
			- ACS_18_5YR_B25002/ACS_18_5YR_B25002_with_ann.csv

		(4) Home ownership data (Census/ACS)
			Downloaded from: https://factfinder.census.gov
			Stored in: raw/census/owner/cdp/
			File(s):
			- ACS_10_5YR_B25026/ACS_10_5YR_B25026_with_ann.csv
			- ACS_11_5YR_B25026/ACS_11_5YR_B25026_with_ann.csv
			- ACS_12_5YR_B25026/ACS_12_5YR_B25026_with_ann.csv
			- ACS_13_5YR_B25026/ACS_13_5YR_B25026_with_ann.csv
			- ACS_14_5YR_B25026/ACS_14_5YR_B25026_with_ann.csv
			- ACS_15_5YR_B25026/ACS_15_5YR_B25026_with_ann.csv
			- ACS_16_5YR_B25026/ACS_16_5YR_B25026_with_ann.csv
			- ACS_17_5YR_B25026/ACS_17_5YR_B25026_with_ann.csv
			- ACS_18_5YR_B25026/ACS_18_5YR_B25026_with_ann.csv

		(5) Home value data (Census/ACS)
			Downloaded from: https://factfinder.census.gov
			Stored in: raw/census/value/cdp/
			File(s):
			- ACS_10_5YR_B25077/ACS_10_5YR_B25077_with_ann.csv
			- ACS_11_5YR_B25077/ACS_11_5YR_B25077_with_ann.csv
			- ACS_12_5YR_B25077/ACS_12_5YR_B25077_with_ann.csv
			- ACS_13_5YR_B25077/ACS_13_5YR_B25077_with_ann.csv
			- ACS_14_5YR_B25077/ACS_14_5YR_B25077_with_ann.csv
			- ACS_15_5YR_B25077/ACS_15_5YR_B25077_with_ann.csv
			- ACS_16_5YR_B25077/ACS_16_5YR_B25077_with_ann.csv
			- ACS_17_5YR_B25077/ACS_17_5YR_B25077_with_ann.csv
			- ACS_18_5YR_B25077/ACS_18_5YR_B25077_with_ann.csv

		(6) Population density data (Census/ACS)
			Downloaded from: https://factfinder.census.gov
			Stored in: raw/census/density/
			File(s):
			- DEC_10_SF1_GCTPH1.ST10_with_ann.csv

		(7) Segregation data (Trounstine 2016)
			Downloaded from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/4LZXTY
			Stored in: raw/
			File(s):
			- trounstineTheil.tab

		(8) Election data
			Downloaded from: http://csus-dspace.calstate.edu/handle/10211.3/210187. 
			Stored in: raw/ceda/
			File(s): 
			- CEDA2019Data.xlsx
			- CEDA2018Data.xlsx
			- CEDA2017Data.xlsx
			- CEDA2016Data.xlsx
			- CEDA2015Data.xlsx
			- CEDA2014Data.xlsx
			- CEDA2013Data.xlsx
			- CEDA2012Data.xlsx
			- CEDA2011Data.xlsx
			- CEDA2010Data.xls
			- CEDA2009Data.xls
			- CEDA2008Data.xls
			- CEDA2007Data.xls
			- CEDA2006Data.xls
			- CEDA2005Data.xls
			- CEDA2004Data.xls
			- CEDA2003Data.xls
			- CEDA2002Data.xls
			- CEDA2001data.xls
			- CEDA2000Data.xls
			- CEDA1999Data.xls
			- CEDA1998Data.xls

	The above inputs are processed by the following scripts. Please run them in the order listed below to reproduce the analysis dataset. 

		(1) scripts/agg/01_aggregate_reports.R
			Processes and combines housing data
			Inputs: All datasets in (I) above
			Output: output/temp/housing_apr.csv

		(2) scripts/agg/02_census_inst.R
			Processes and combines Census data
			Inputs: All datasets in (II) and (III, 1-7) above
			Output: output/temp/city.csv 

		(3) scripts/agg/03_ceda_clean.R
			Processes and combines CEDA election data 
			Inputs: All datasets listed in (III, 8) above 
			Output: output/temp/ceda.csv

		(4) scripts/agg/04_ceda_clean_sf.R
			Processes and combines CEDA election data for San Francisco
			Inputs: All datasets listed in (III, 8) above 
			Output: output/temp/ceda_sf.csv

		(5) scripts/agg/05_elec_wru.R
			Codes ethnicities in election data; creates variables for analysis; aggregates to city-election
			Inputs: 
			- output/temp/ceda.csv
			- output/temp/city.csv
			Output: output/temp/ceda_agg.csv

		(6) scripts/agg/06_elec_wru_sf.R
			Codes ethnicities in election data; creates variables for analysis; aggregates to city-election for San Francisco
			Inputs: 
			- output/temp/ceda.csv
			- output/temp/city.csv
			Output: output/temp/ceda_agg_sf.csv

		(3) scripts/agg/07_merge_agg.R
			Inputs: 
			- output/temp/housing_apr.csv
			- output/temp/city.csv
			- output/temp/ceda_agg.csv
			- output/temp/ceda_agg_sf.csv
			Output: output/final/housing_agg.csv

(2) Distributive analysis for the case study cities (housing_spatial.csv)

	This dataset is constructed from the authors' original data collection on permits as found in city council minutes, in addition to block group-level controls from the Census.  

	Specifically, it is comprised of the following inputs: 

	I. DEPENDENT VARIABLES

		(1) Housing permits, collected by the authors from city council minutes 
			Stored in: raw/spatialPermits
			- cvraPermits - anaheimFinal.csv
			- cvraPermits - escondidoFinal.csv
			- cvraPermits - glendaleFinal.csv
			- cvraPermits - santaBarbaraFinal.csv
			- cvraPermits - santaCruzFinal.csv
			- cvraPermits - venturaFinal.csv

	II. SHAPEFILES

		(1) California block group shapefile from 2015, used for assigning addresses to block groups
			Downloaded from: https://www2.census.gov/geo/tiger/TGRGDB15/
			Stored in: raw/shapefiles/
			File(s):
			- tl_2015_06_bg

	III. COVARIATES

		(1) Population and race data (Census/ACS)
			Downloaded from: https://factfinder.census.gov
			Stored in: raw/census/race/bg/
			File(s):
			- ACS_13_5YR_B11001H/ACS_13_5YR_B11001H_with_ann.csv
			- ACS_14_5YR_B11001H/ACS_14_5YR_B11001H_with_ann.csv
			- ACS_15_5YR_B11001H/ACS_15_5YR_B11001H_with_ann.csv
			- ACS_16_5YR_B11001H/ACS_16_5YR_B11001H_with_ann.csv
			- ACS_17_5YR_B11001H/ACS_17_5YR_B11001H_with_ann.csv
			- latino/ACS_13_5YR_B11001I_with_ann.csv
			- latino/ACS_14_5YR_B11001I_with_ann.csv
			- latino/ACS_15_5YR_B11001I_with_ann.csv
			- latino/ACS_16_5YR_B11001I_with_ann.csv
			- latino/ACS_17_5YR_B11001I_with_ann.csv

		(2) Income data (Census/ACS)
			Downloaded from: https://factfinder.census.gov
			Stored in: raw/census/income/bg/
			File(s):
			- ACS_13_5YR_B19013/ACS_13_5YR_B19013_with_ann.csv
			- ACS_14_5YR_B19013/ACS_14_5YR_B19013_with_ann.csv
			- ACS_15_5YR_B19013/ACS_15_5YR_B19013_with_ann.csv
			- ACS_16_5YR_B19013/ACS_16_5YR_B19013_with_ann.csv
			- ACS_17_5YR_B19013/ACS_17_5YR_B19013_with_ann.csv

		(3) Occupancy data (Census/ACS)
			Downloaded from: https://factfinder.census.gov
			Stored in: raw/census/occupancy/bg/
			File(s):
			- ACS_13_5YR_B25002/ACS_13_5YR_B25002_with_ann.csv
			- ACS_14_5YR_B25002/ACS_14_5YR_B25002_with_ann.csv
			- ACS_15_5YR_B25002/ACS_15_5YR_B25002_with_ann.csv
			- ACS_16_5YR_B25002/ACS_16_5YR_B25002_with_ann.csv
			- ACS_17_5YR_B25002/ACS_17_5YR_B25002_with_ann.csv

		(4) Home ownership data (Census/ACS)
			Downloaded from: https://factfinder.census.gov
			Stored in: raw/census/owner/bg/
			File(s):
			- ACS_13_5YR_B25008/ACS_13_5YR_B25008_with_ann.csv
			- ACS_14_5YR_B25008/ACS_14_5YR_B25008_with_ann.csv
			- ACS_15_5YR_B25008/ACS_15_5YR_B25008_with_ann.csv
			- ACS_16_5YR_B25008/ACS_16_5YR_B25008_with_ann.csv
			- ACS_17_5YR_B25008/ACS_17_5YR_B25008_with_ann.csv

		(5) Home value data (Census/ACS)
			Downloaded from: https://factfinder.census.gov
			Stored in: raw/census/value/bg/
			File(s):
			- ACS_13_5YR_B25077/ACS_13_5YR_B25077_with_ann.csv
			- ACS_14_5YR_B25077/ACS_14_5YR_B25077_with_ann.csv
			- ACS_15_5YR_B25077/ACS_15_5YR_B25077_with_ann.csv
			- ACS_16_5YR_B25077/ACS_16_5YR_B25077_with_ann.csv
			- ACS_17_5YR_B25077/ACS_17_5YR_B25077_with_ann.csv

	The above inputs are processed by the following scripts:

		(1) scripts/spatial/spatial_prepare.R
			Inputs: All datasets in (I)-(III) above
			Outputs: output/final/housing_spatial.csv

		(2) scripts/spatial/getGeoDetails.R
			Not independently run; used in spatial_prepare.R to geocode addresses. Adapted from Shane Lynn: https://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/

Additionally, we include the scripts we used to download meeting minutes, in scripts/minutes. These include: 
	- anaheim.py
	- chulavista.py
	- compton.py
	- escondido.py
	- glendale.py
	- ventura.py