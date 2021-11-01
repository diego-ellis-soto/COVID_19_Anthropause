# COVID-19 Animal Movement Project Notes

## TODO:
* Can't connect to github remote - need top push, clomne onto HPC and run test across individuals
* Once available, modify par_cougar.r to run from a mosey database
* Currently no data cleaning - need to automate some sort of outlier, QA/QC check
* Currently running `ctmm` on segments with no guarantee that assumptions are being met (e.g., range residence for akde)
  * Need to add some automatic segmentation steps - M4? SegClust2D?
* Once we have a semi-complete product from "par_cougar.r", build plotting script to vsiualize outputs
  

## Activity log

|Date|Activity|
|:-|:------------|
|2021-11-01|Scott translating olympic_cougar.r into parallel version|



## Notes
Currently using the Olympic Cougar dataset to build workflow out and increase complexity/species counts. See TODOs above for glaring gaps in pipeline...  Need to add those functionalities and build out to operate across multiple datasets.

*Primary Analyses* 
Compare all products across lockdown conditions (before, during, after).
**Products**:
* Movement metrics:
  * NSD
  * Step length
  * Turn angle
* AKDE (HR size)
* RSF (use `ctmm`?)
  * Only basic variables (e.g., NDVI, temp, elev, HFI, human mobility)
* Niches
  * Niche breadth
  * Pairwise niche dissimilarity\
  
*Parallelization Strategy*
The script parallelized (via `foreach`) across individuals.  Could use a job submit script a la my kiwa survival project (from Shubhi) to submit one job per species in the mosey?