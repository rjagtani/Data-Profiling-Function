# Data-Profiling-Function
This function generates a data profile report of the dataset passed as an argument and writes three files for the variable types numerical, categorical and date in the working directory. This is an improvement over the package dataQualityR's function checkDataQuality as it takes care of the following issues : 
1. Checkdataquality treats date variables as numeric.This function generates a separate report for datetime variables
2. CheckDataQuality throws an error when there is only one variable of categorical or numerical type. This bug has been taken care of in this function.
3. CheckDataQuality does not work for other datatypes such as tibbles and data tables.


The function takes the following arguments : 
1. data : the dataframe for which summary files will be generated
2. top_n = 5 : integer; the top n frequencies for a categorical variable will be written to the summary file. By default takes the value 5
3. numeric.cutoff : The minimum number of unique values needed for a numeric variable to be treated as continous. This feature is included to account for binary or multiclass variables with small number of unique values,which are stored as numeric while reading file from csv/xlsx. Default is -1 which does not place any cut-off and all numeric variables are treated as continuous.
4. datetime_format : Optional; to be provided if date columns are present in the dataset.
5. numerical_cols : Optional;character vector of column names to be provided if variables need to be coerced to numeric type
6. numerical_cols : Optional;character vector of column names to be provided if variables need to be coerced to character/factor type
7. numerical_cols : Optional;character vector of column names to be provided if variables need to be coerced to date type.


Example : 

data_profile(data=b,datetime_format = '%b%d%Y',categorical_cols =c('engineRun','EventCode'))

