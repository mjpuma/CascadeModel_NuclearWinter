# Cascade propagation model for food shocks 
## Marchand et al. 2016 model
-  Scripts to run the Marchand cascade model for nuclear winter scenario as described in the PNAS article "A regional nuclear conflict would compromise global food security" by Jonas Jägermeyr, Alan Robock , Joshua Elliott , Christoph Müller , Lili Xia , Nikolay Khabarov , Christian Folberth , Erwin Schmid , Wenfeng Liu , Florian Zabel , Sam S. Rabin , Michael J. Puma , Alison Heslin , James Franke , Ian T. Foster , Senthold Asseng , Charles G. Bardeen , Owen B. Toon , Cynthia Rosenzweig.
- Contains: 1) Preprocessing scripts tailored for Jägermeyr et al. (2020) experiment; 2) cascade functions from original Marchand et al. (2016) model.
- Original model (cascade functions plus original model preprocessing scripts) are also available at https://github.com/pmarchand1/cereals-network-shocks and described in the paper by Marchand *et al.* [Reserves and trade jointly determine exposure to food supply shocks](http://iopscience.iop.org/article/10.1088/1748-9326/11/9/095009).

## Model Vignette
- INPUT (required): Bilateral trade of commodities at country level, production/consumption/storage for all countries
- INPUT (ancillary): Commodity list, country list, conversion factors from commodity mass to common units (e.g. kcal, protein, US dollars)
- OUTPUT: Changes in stocks and consumption at country level
- CODE: Written in R
- RUNTIME: few minutes on desktop computer
- RESOLUTION: Country level

## Model Scripts
### Preprocessing: *MatrixCreationMaize.R* and *MatrixCreationWheat.R*
Creates trade, production, and reserves matrices for use in cascade model with maize and wheat commodities, respectively. Requires existing files in *ancillary* and *inputs* directory.

### Running model: *MaizeAnalysis.R* and *WheatAnalysis.R*
These are the main scripts for running FSC, calling functions in *CascadeFunction.R*, for maize and wheat commodities, respectively.

### *CascadeFunction.R*
Functions for shock propagation model in food trade networks

## Input files
- Trade data from FAOSTAT: detailed trail matrix, normalized, all data. The trade matrix is available here - http://www.fao.org/faostat/en/#data/TM - on the right side bar under "Bulk Downloads", select "All Data Normalized".   *trade_dat <- read.csv("Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"*
- Production data from FAOSTAT: production quantity in tonnes. The crop production data are available at: http://www.fao.org/faostat/en/#data/QC. *prod_dat<-read.csv("productiondataFAOSTAT.csv")*
- Reserves data from USDA: downloadable dataset - psd grains pulses. The stocks data are available here - https://apps.fas.usda.gov/psdonline/app/index.html#/app/downloads - listed as "Grains" the file is called "psd_grains_pulses_csv.zip". *psd <- read.csv("psd_grains_pulses.csv")*

## Ancillary files
- *CropCommodities.csv*: crop commodities inlcuded in the analyses
- *Countries162.csv*= FAO country code list for those countries included in analyses 
- *crop_list.csv*:  kcal conversions for various crops
- *ciso.csv* = country codes
