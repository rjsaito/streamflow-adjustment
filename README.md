# Streamflow Adjustment

R source scripts of various processes and analyses related to streamflow adjustment, related to Scientific Investigations Reports by the US Geological Survey.

http://dx.doi.org/10.3133/sir20165049

http://dx.doi.org/10.3133/sir20165050



# Scripts 

**LP3_Quantiles_Regression_Combinations_RegSkew_Saito.R**

A make-shift all-subset regression model with constrained variable groups (Drainage Area, Urbanization Fraction, Wetland, Slope, Impervious, etc),
 with different transformations of variables.
 

  
  

**PeakFQ.Quantiles.R**

Quantiles of Estimated Peak Discharges, extracted from the Estimated Moments Algorithm results computed by the USGS Software "PeakFQ"


**QR_New.Adjustment_PhaseII.R**



Estimations of annual peak discharges, adjusted for temporal changes in urbananization fraction, computed using a bootstrapped panel quantile regression.


**SegPeakQR.R**

Application of simple quantile regression model of peak dischrarges on urbanization and precipitation.

**date.convert.R**

Data conversion function for USGS Water Years.


**peakfq.formatting.RA.R**

Data formatting/preparation program to convert data to format to be used for USGS Software "PeakFQ"


**psf.tweak.R**

Program to tweak parameters in the specification file for USGS software "PeakFQ"


**segmentprep.new.R**

Segmentation of streamgage records, defined by a scoring algorithm that considers drainage area, basin storage, length of record in years, downstream/upstream relationship, etc.

**with.dam.segments.noDet.newUrban.QRPanel_data.only.mod_new.R**

Data preparation for temporal adjustment and estimation of annal peak discharge quantiles.


