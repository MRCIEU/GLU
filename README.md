# GLU: A tool for analysing continuously measured glucose in epidemiology

GLU is described in the paper:
L.A.C. Millard et al. GLU: A tool for analysing continuously measured glucose in epidemiology, bioRxiv, 2018



## 1. Overview

GLU takes continuous glucose monitoring (CGM) data as input, and derives a set of characteristics describing these data:

1. Median absolute deviation (MAD)
2. Area under the curve (AUC)
3. Proportion of time spent in low, normal and high values
4. Standardised glycaemic variability percentage (sGVP)
5. Fasting proxy
6. Meal statistics:
    1. Time to peak
    2. 1-hour postprandial AUC
    3. 2-hour postprandial AUC
7. Medication statistics
    1. 1-hour postprandial AUC
    2. 2-hour postprandial AUC
8. Exercise statistics
    1. 1-hour postprandial AUC
    2. 2-hour postprandial AUC


### 1.1 Requirements

This code has been tested with R-3.3.1-ATLAS and requires the packages "optparse" and "ggplot2",
which can be installed by running the following commands in R:

```R
install.packages("optparse")
install.packages("ggplot2")
```


## 2. Running GLU

There are two steps to generate the summary CGM variables:

1. Preprocess the CGM data files into the appropriate format
2. QC and deriving summary characteristics


### 2.1 Step 1: preprocessing


#### Preprocessing Medtronic iPro2 CGM data

If you are using CGM data from a Medtronic iPro2 device, you can use the following command to preprocess all CGM files in a directory:

```bash
sh mainConvertFileFormat.sh [dataDir] [outDir] [filename]
```

The `filename` argument is optional.

This step converts the Medtronic ipro2 data files to the format required by step 2, by:

1. Convert file encoding
2. Remove header lines

The data files should have the name "data_export-[USERID].[EXTENSION]" (e.g. data_export-1234.tsv), this is the naming convention used when data is exported from Medtronic [here](https://carelink.minimed.eu/ipro/hcp/login.jsf).


#### Preprocessing other data formats

If using GLU with CGM data from other devices, you can convert your data to a TSV (tab seperated values) file with the following columns, and then run step 2.


Required columns:

Column | Description
-------|--------
Timestamp          | Required in format 'dd/mm/yy HH:MM:SS'.
Sensor Glucose (mmol/L)          | Sensor glucose levels.


Optional columns:

Column | Description
-------|--------
Meal          | Optional column, which if not blank indicates a meal timepoint.
Exercise          | Optional column, which if not blank indicates a exercise timepoint.
Medication          | Optional column, which if not blank indicates a medication timepoint.
BG Reading (mmol/L)    | Blood glucose levels.
Excluded	| Whether timepoint should be removed pre-analysis.

See data used in the examples below.

NB: If you are using a common format you can send us some example data and we can add preprocessing for this format to the GLU tool.

### 2.2 Step 2: QC and deriving characteristics

Perform QC and derive characteristics/graphs for all CGM TSV files in a directory.


```bash
cd R/
Rscript runGLU.R \
--indir="<indir>" \
--outdir="<outdir>" \
--filename="<filename>" \
--pregnancy
```

The `filename`, `outdir` and `pregnancy` arguments are optional.

#### Required arguments

Arg | Description
-------|--------
inDir   | Directory where processed CGM data files (i.e. output from step 1) are stored.

#### Optional arguments

Arg | Description
-------|--------
filename          | Run GLU for 1 participants, with user ID `userID'.
pregnancy	| Set to `TRUE` to use pregnancy thresholds for proportion of time spent in low, normal and high characteristics.
outDir  | Directory where derived CGM characteristics data files should be stored. Default is `inDir`.
nightstart	| Time night period starts (HH:MM). Default is 23:00.
daystart	| Time day period starts (HH:MM). Default is 06:30.
missingsummary	| Set to `TRUE` to generate a summary of missingness (`missingSummary.csv`).
save 	|	Set to `TRUE` to save derived (resampled) CGM data.
freq	|	Frequency between time points in CGM data (minutes). Default is 5 minutes.
impute	|	Use simple imputation method (see paper).
hypothreshold	| Threshold between hypoglycaemia and euglycemia, to overide the defaults
hyperthreshold	| Threshold between euglycemia and hyperglycaemia, to overide the defaults
timeformat	| Time format in CGM data. Default='%d/%m/%y %H:%M:%S' (format for Medtronic ipro2 data).



## 3. Description of GLU QC

We remove all rows without either a sensor glucose value or an event (meal/exercise/medication). We resample to 1 minute epochs.

1. Outlier detection

Outlier detection is performed as follows:
- Calculate the difference of each SG value from the previous and next values in the SG trace.
- Calculate the SD of the distribution of these differences
- Identify time points where these differences are > 4SD from the mean difference; these are potential outliers that should be checked.


2. Dealing with missing data

The `freq' argument can be used to specify the epoch frequency (interval between adjacent timepoints) in the CGM data.

A day is included if there are `1440/freq' SG values, as this means that there are no missing SG values.
The derived variables below are only calculated using the included days.

The `impute' argument can be used to impute missing blocks in the CGM data, using an `approximal' imputation approach (see GLU paper for more information).


3. Manual review

We recommend that researchers visually review each participant's data.
GLU outputs CGM trace plots and Poincare plots to do this, and examples can be found in the `examples` subdirectory.


## 4. Description of GLU derived variables


1. Median absolute deviation (MAD)

The median of the absolute difference of each sensor glucose value from the median sensor glucose value.

We calculate this for each day and then calculate the mean across all days, and across all nighttime and daytime periods, respectively.


2. AUC, per minute for each valid day, and mean across all valid days
 - The AUC of the SG values
 - Trapezoid method (linear interpolation between the discrete glucose measurements) as described [here](https://www.boomer.org/c/php/pk0204a.php).

 We calculate this for each day and then calculate the mean across all days, and across all nighttime and daytime periods, respectively.

3. Proportion of time spent in low, normal and high SG ranges, per valid day and mean across all valid days

Different thresholds are appropriate for pregnant women versus the general population.

Pregnancy thresholds: low<3.5 mmol/l, 3.5<=normal<=7.8 mmol/l, high >7.8 mmol/l.
General population thresholds: hypoglycemia<3.9 mmol/l, 3.9<=normal<10.0 mmol/l, hyperglycemia>= 10.0 mmol/l.

To calculate these proportions we linearly interpolate between adjacent SG values to calculate the proportion of time within each range.

The following pseudocode (for pregnancy thresholds) demonstrates the process we use.

 ```
 lowT=3.5 # threshold value between low and normal ranges
 highT=7.8 # threshold value between normal and high ranges
 INIT lowTime=0; normalTime=0; highTime=0;
 SGmin=min(SG(i), SG(i+1))
 SGmax=max(SG(i), SG(i+1))

 # both SGmin and SGmax are in same range (either low, normal or high)
 IF SGmin<lowT & SGmax<lowT THEN lowTime+=T(i+1)-T(i); # both in low range
 ELSE IF SGmin>=lowT & SGmin<=highT & ISIGmax>=lowT & SGmax<=7.8 THEN normalTime+= T(i+1)-T(i); # both in normal range
 ELSE IF SGmin>highT & SGmax>highT THEN highTime+= T(i+1)-T(i); # both in high range

 ELSE IF SGmin<lowT THEN # SGmin value is in low range and SGmax is in either mid or high range
	lowTime+=(T(i+1)-T(i))*(lowT-SGmin)/(SGmax-SGmin)
	IF SGmax<=highT THEN 
		normalTime+=(T(i+1)-T(i))*(SGmax-lowT)/(SGmax-SGmin)
	ELSE
		normalTime+=(T(i+1)-T(i))*(highT-lowT)/(SGmax-SGmin)
		highTime+=(T(i+1)-T(i))*(SGmax-highT)/(SGmax-SGmin)

 ELSE IF SGmin<=highT THEN # ISIG values are in normal and high ranges resp.
	normalTime+=(T(i+1)-T(i))*(highT-SGmin)/(SGmax-SGmin)
	highTime+=(T(i+1)-T(i))*(SGmax-highT)/(SGmax-SGmin)

 RETURN lowTime, normalTime, highTime
 ```

 where `i` is the index along the SG sequence, `T(i)` is the actual time at position `i`.

 We calculate this for each day and then calculate the mean across all days, and across all night-time and day-time periods, respectively.


4. Standardised glycaemic variability percentage (sGVP), per included day and mean across all included days


Intuitively, sGVP is based on the length of the line of the glucose trace. 
GVP is the length of the line relative to the length of a completely flat glucose trace. 
A trace with no variability has GVP=0, and the more variability (both in amplitude and undulations) from a flat trace, the higher the GVP value.
The sGVP measure is the GVP calculated using a standardised glucose trace, to reflect the degree a trace undulates.
Standardisation is performed as:

``` 
(SG - median(SG))/MAD
```


We calculate this for each day and then calculate the mean across all days, and across all night-time and day-time periods, respectively.


5. Nocturnal Glucose

The lowest mean 30 minute consecutive period during the night-time.


6. Meal time to peak

The number of minutes from the meal to the next peak SG.

In the simple case the peak is the nearest subsequent time t, after the meal where SG(t-1) < SG(t) > SG(t+1).

In the case where the peak is on a plateau, then (i.e. there are multiple timepoints on the peak with the same peak value) 
then we define the peak time point as the nearest time point to the meal on this plateau.


7. Post-event AUC

An event is either eating, exercise or medication.

Post-event AUC can be either 1-hour or 2-hour. E.g. 1-hr postprandial AUC, 1-hr post-exercise AUC, 1-hr post-medication AUC etc.

The `n`-hr post-event AUC is the mean of the 3 consecutive SG readings `n` hrs after the event.




## 5. Example CGM data and derived variables

The `examples` directory contains an example for CGM data of two people.

