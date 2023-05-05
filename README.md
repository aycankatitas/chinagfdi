# chinafdi
Replication data for [“China's Rise - Threat or Opportunity? The Pacifying Effect of Chinese Foreign Direct Investment on American Threat Perceptions”](https://www.dropbox.com/s/ct2gtysjowft747/Katitas_ChinaFDI.pdf?dl=0)  [Online Appendix](https://www.dropbox.com/s/imu0x588zbvpjzm/Katitas_ChinaFDIapp.pdf?dl=0) 

## Repository Layout 
This repository is split into several folders: ``code``, ``figures``, and ``results``. 

- [ ] ``code``: This folder contains all scripts needed to be run in order to generate all data.
- [ ] ``figures``: All R code for replicating the figures in the paper, as well as the resulting pngs. 
	- [ ] ``results``: Intermediary data created by scripts in the ``code`` folder. 

## Installation 
Recreating all tables and figures in this repository requires working installations of [R]([Index of /src/base/R-4](https://cran.r-project.org/src/base/R-4/)  All this coded was tested R version 4.1.3 on a Mac Monterey. 

### R dependencies 
All R code uses the following packages: ``dplyr, tidyverse, lfe, stargazer, broom, marginaleffects, modelsummary, readstata13, scales, foreign, countrycode, quantmod, lmtest, sandwich, matchit, cobalt`` 

## Replication
The replication requires the researcher to collect a number of public and private dataset. Information on how to acquire the necessary datasets are listed below. 

### Public 

- [ ] [ANES | American National Election Studies 2012 and 2020](https://electionstudies.org/)
- [ ] [County-level unemployment data - Bureau of Labor Statistics](https://www.bls.gov/lau/)
- [ ] I collected congressional district level US Census data using [CRAN - Package censusapi](https://cran.r-project.org/web/packages/censusapi/index.html). You need to get an API key from US Census to use this package. 
- [ ] [County-level manufacturing employment data - County Business Patterns](https://www.census.gov/programs-surveys/cbp.html)
- [ ] [Dave Leip’s Atlas - Congressional district level Presidential vote share](https://uselectionatlas.org/)
- [ ] China Trade Shock data - [The China Syndrome: Local Labor Market Effects of Import Competition in the United States - American Economic Association](https://www.aeaweb.org/articles?id=10.1257/aer.103.6.2121)

### Private

- [ ] The project includes proprietary data on greenfield foreign direct investment in the United States from Financial Times’ fDi Markets data. More information on how to acquire the necessary dataset is [here](https://www.fdimarkets.com/). 
- [ ] I have collected local newspaper articles on Chinese greenfield investment announcements using NewsBank’s [Access World News](https://www.newsbank.com/libraries/military/solutions/access-world-news). In the appendix, I explain the step-by-step process of how I collected the articles. ``newsbank`` folder in ``code`` provides the code to scrape the site. 
- [ ] The project includes proprietary data on mergers and acquisition transactions in the United States from Refinitiv’s SDC Platinum. More information on how to acquire the necessary dataset is [here](https://www.refinitiv.com/en/products/sdc-platinum-financial-securities). 








