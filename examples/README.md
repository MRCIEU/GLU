


# Example CGM data and GLU commands

The `original/` directory contains the original Medtronic data files.
The `derived/` directory contains the two CGM data files generated from step 1,
and the output from step 2, cgmSummary.csv and cgmSummaryVerbose.csv.

To try the tool with this data use the following commands.

### Running GLU

First change directory to GLU R code directory:
```bash
cd ../R/
```

#### Medtronic iPro2 data

For both data files:
```bash
Rscript runGLU.R --indir="../examples/medtronic-ipro2/original/" --outdir="../examples/medtronic-ipro2/derived/"
```

For a specific data file:
```bash
Rscript runGLU.R --indir="../examples/medtronic-ipro2/original/"  --outdir="../examples/medtronic-ipro2/derived/" --filename="data_export-999999.csv"
```

#### Abbott Freestyle Libre data

```bash
Rscript runGLU.R --indir="../examples/freestyle-libre/original/"  --outdir="../examples/freestyle-libre/derived/" --filename="Healthy_volunteer_example_data_FreeStyle_Libre.txt" --device=2 --freq=15 --nightstart='00:00' --impute
```


#### Dexcom G2 data

```bash
Rscript runGLU.R --indir="../examples/dexcom-g2/original/"  --outdir="../examples/dexcom-g2/derived/" --filename="CLARITY_Export__111111-example.csv" --device=1 --nightstart='16:30'
```

