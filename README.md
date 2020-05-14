# Data Archive

This repository serves as research archive for the Master's thesis: "The Influence of Teacher-based Routing on the Accuracy and Classification of Multistage Tests" for the masters programme Methodology & Statistics for the Behavioural, Biomedical and Social Science at Utrecht University. The final data simulation was run on may 7th-9th, 2020 by Marie Buijs. 

## *Thesis details*

| Title  |  The Influence of Teacher-based Routing on the Accuracy and Classification of Multistage Tests   |
|---|---|
| Author | M. Buijs |
| Supervisors  | Dr. D. Hessen, Prof. dr. A.G.J. Van de Schoot, K. Lek, MSc., Dr. R. Feskens |  
| Department  |  Department of Methodology & Statistics, Utrecht University & Stichting Cito  | 
| Final simulations | May 7th-9th, 2020 |   
| Manuscript submitted  |  May 2020  | 

### *Abstract*
Multistage testing (MST) is a test administration method where the difficulty of a test adapts to the ability of each examinee. In the first stage of traditional MST designs, all students take the same module to determine the initial ability on which module allocation in the subsequent stage(s) is based. Routing based on the teacher’s judgment may be an interesting alternative to the regular routing stage in MST designs since it resolves the need of the regular routing stage. In this paper, we investigated the effect of replacing the routing stage of a multistage test with teacher-based routing. To make this study as realistic as possible, the simulations were based on the Dutch End of Primary School Test (EPST), which provides track recommendations. Through simulations, we compared the precision of ability estimates and the differences in EPST track recommendation with and without teacher-based routing. The number of modules available in the first stage were varied. Furthermore, there was a focus on the situation in which module allocation based on the teacher was not in line with student’s performance. Results indicate that the teacher-based routing design provides students with slightly less precise ability estimates than the regular-routing EPST. Most students obtain an appropriate track recommendation when teacher-based routing is implemented, although this is less often the case for students who were misdirected in the first stage. In conclusion, the gain of information by assigning students to a module that matches their ability does not completely make up for the loss of information by shortening the test. However, with further research, the teacher-based routing MST might be implementable in high-stakes situations as well. 

## Data storage
*Data management plan*
Description of organization of data management, responsibilities, contents of the archive (including version number and date)

*Brief description of hypothesis, research design, conceptual framework, method followed and structure of data collected*
An electronic version of the accepted or published manuscript is sufficient for this

*instructions, procedures, the design of the experiment and any stimulus materials reasonably required for replication.*
The materials must be available in the language in which the research was conducted. An English translation is optional (but appreciated); 

*raw data files (the most direct record of the behaviour or responses of test subjects/respondents, e.g.: an unfiltered export of an online survey or raw time series for an EEG measurement, e-data files for an E-Prime behavioural experiment).*
In case of a simulation study, it depends on the size of the data files whether they are stored in the archive or whether only the procedure to generate the data is provided; 

*computer code (e.g. SPSS syntax file, MATLAB analysis scripts)*
that describes the steps to obtain the final analysis data from the raw data. This should include brief explanations of the steps in English; 

*data files that have been subjected to a final analysis for the purpose of writing the article (e.g. SPSS data file after transformation of variables, changes to selections, etc.).*
The latter is not necessary if the raw data file has been analyzed;

*In case of a simulation study, the computer code that has been applied to simulate the data, including version number of the software, operation system of the machine, and the seed number of the random generator.*
This should include brief explanations of the steps in English;  

*computer code (e.g. SPSS syntax file) describing the steps followed to arrive at the results in the manuscript, based on the analysis data.*

*In case a data management plan is missing, a Readme file is needed that describes where the documents and/or files can be found and how they should be **interpreted**.*
The Readme file should also contain the following information: 
a.	the name of the person who saved the documents and/or files; 
b.	the date on which the manuscript was accepted, including a reference; 
c.	the date on/period in which the data was collected or simulated; 
The Readme file must be sufficiently clear to enable the results in the publication to be reproduced on the basis of the components of the research archive package.


### *Contents*

| Files/Folders  |  Description   |
|---|---|
| Data  | Folder with simulated data (not included due to size)  |  
| Documents  |  Folder containing the thesis and ethical clearance form.  | 
| Figures  | Folder with the figures used in the manuscript  |   
| Research Report  |  Folder containing the research report and analysis pertaining to appendix A  | 
| Results  |  Folder where results are stored (not included due to size) | 
| SourceFiles  | Folder containing files that are sourced (including a seperate ReadMe file) | 
| Table for simulation  | Folder containing the table used as basis for the data simulation | 
| 1.Data Simulation.R  | Contains script used for data simulation.  | 
| 2. Simulation Designs.R  |  Contains script used for simulation of different teacher-based routing MST designs. | 
| 3. Assumptions.R  |  Contains script checking assumptions. | 
| 4a. SEM.R | Contains script analyzing standard error of measurement.  | 
| 4b. Track placement recommendations.R |  Contains script analyzing the track placement recommendations. | 

### *Replicating the analysis*

To replicate the findings in the paper, run "1. Data Simulation.R", "2. Simulation Designs.R", "3. Assumptions.R", "4a. SEM.R", "4b. Track placement recommendations.R" in this order. (see "1. Data Simulation.R" for more details on the version number of the software and operation system of the machine). 

## Privacy 

The simulations are based on non-public microdata from Statistic Netherlands (CBS). The microdata were analyzed in the remote secured online environment of CBS. Before any results were exported from the remote secured online environment of Statistics Netherlands, the CBS tested the results to prevent publication of any identifiable information about individuals. 
For this thesis, only the contingency table from the teacher's recommendation and the EPST recommendation was used. No information about individuals can be obtained from this table. 
For the research report an additional table containing the placement in the third year of secondary education for students with 3 or more levels mismatch between the teacher's recommendation and the EPST recommendation was used. No specific teacher's recommendation and/or EPST recommendations are mentioned in this table, thus no information about individuals can be obtained from (the combination of ) this table(s). 

## Permission and access

### *Permission*

This study is approved by the Ethics Committee of the Faculty of Social and Behavioural Sciences of Utrecht University, filed under number 19-215. A copy of the approval form can be found in the Documents folder. 

### *Acces*

The research archive is available through https://github.com/MJBuijs/Data-Archive-Thesis-Teacherbased-Routing. The Github repository is public and access to the research archive is open-source. A copy of this repository has been stored in the archives of Utrecht University. The data will be stored for a minimal duration of ten years.



