# GPT_Ordinary_Meaning
Engel/McAdams: Asking GPT for the Ordinary Meaning of Statutory Terms, Illinois Journal of Law, Technology, &amp; Policy 2024

1.	Raw Data

Raw data, i.e. the responses given by GPT 3.5 turbo, to each of the prompts defined in the draft, are in the folder “responses”.

2.	Data Preparation

The script “EM240120DataPreparation.R” extracts the raw data, and prepares it for analysis. The first two blocks of code explain the logic of data preparation, for an individual response file. The actual data preparation is -  separately for each prompt – done in a function. From each set of 25 test objects per prompt, we generate one data file (in RData format). 

To the extent that we compare our data with the data from Kevin Tobia, Harvard Law Review 2020, we use the data prepared in the script “TobiaDataPreparation240120.R”.

3.	Data Analysis

The code for all data analysis is in script “EM240120Analysis.R”.

