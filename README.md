# FileCompareR
This script takes two data files (.tsv, .csv, .xlsx), a clinical and a metadata file in most cases, and compares two selected columns.

To run the script on a CDS v1.3.1 template, run the following command in a terminal where R is installed for help.

```
Rscript --vanilla FileCompareR.R --help
```

```
Usage: FileCompareR.R [options]

FileCompareR v1.0.0

Options:
	-c CHARACTER, --clinical=CHARACTER
		clinical dataset file (.xlsx, .tsv, .csv)

	-m CHARACTER, --metadata=CHARACTER
		metadata dataset file (.xlsx, .tsv, .csv)

	-h, --help
		Show this help message and exit
```
    
An example data set is also given, here is one possible entry:

```
Rscript --vanilla FileCompareR.R -c test_files/clinical_test.xlsx -m test_files/metadata_test.csv 

Based on the following columns, which column should the clincal file use:

participant_id
test1
test2
test3
test4
test5

participant_id

Based on the following columns, which column should the metadata file use:

participant_id_match
participant_id_partial
participant_id_nomatch
info1
info2
info3

participant_id

********
Please select one of the following viable options:
********

participant_id_match
participant_id_partial
participant_id_nomatch
info1
info2
info3

participant_id_partial

Please see the output found here: test_files/clinical_test_Compare_2022_10_12.txt
```
