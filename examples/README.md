


# Example CGM data and GLU commands

The `original/` directory contains the original Medtronic data files.
The `derived/` directory contains the two CGM data files generated from step 1,
and the output from step 2, cgmSummary.csv and cgmSummaryVerbose.csv.

To try the tool with this data use the following commands.

### Running pre-processing for Medtronic ipro2 data

First change directory to GLU root directory:
```bash
cd ..
```

For both data files: 
```bash
sh mainConvertFileFormat.sh examples/original/ examples/derived/
```

For a specific data file:
```bash
sh mainConvertFileFormat.sh examples/original/ examples/derived/ data_export-999999.csv
```

### Running GLU

First change directory to GLU R code directory:
```bash
cd ../R/
```


For both data files:
```bash
Rscript runGLU.R --indir="../examples/derived/"
```

For a specific data file:
```bash
Rscript runGLU.R --indir="../examples/derived/" --filename="data_export-999999.csv"
```


