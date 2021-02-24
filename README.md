# ARPA
Automatic RT-PCR Analysis; A tool for massive detection of COVID-19

## MOTIVATION
ARPA is designed to help the specialized staff working on the detection of COVID-19 thorugh the technique of RT-PCR. ARPA's main goal is to automatize some laborious steps of the process to speed up the analysis and to decrease the response time.

## FUNCTIONALITIES

ARPA has been designed in a modular setting. It allows the automatic analysis of the amplification curves, the visual inspection of the results via a friendly web-interface and the generation of HTML reports with the results from the experiment.

ARPA has several functionalities:

 - It generates and analyzes the amplification curves from fluorescence measured in a RT-PCR experiment
 - It evaluates the control curves to assess the quality of the experiment
 - It evalutes the sample curves to determine the status of each sample
 - It provides a friendly web-interface to look at each amplification curve
 - It provides a friendly web-interface to look at a summary table with the results of the experiment
 - It generates HTML reports with the amplification curves and the status of the plate per each sample

### AUTOMATIC ASSESMENT OF THE EXPERIMENT'S QUALITY AND OF THE AMPLIFICATION CURVES 

The values of the fluorescence per cycle are retrieved from the files exporteD by the RT-PCR equipment. The curves are evaluated. A correct amplification should behave as a sigmoid curve and cross a fluroescence threshold before a chosen cycle.

 - All control samples are evaluated to determine the quality of the experiment.
 - The curves for each sample are assesed to determine the status of each sample


### FRIENDLY WEB-INTERFACE TO LOOK AT THE AMPLIFICATION CURVES AND THE RESULTS OF THE EXPERIMENT

**The web interface allow the user to load the results file generated by the RT-PCR machine**

![alt text](https://github.com/INMEGEN/ARPA/blob/main/images/web-initial.jpeg?raw=true)


**It shows the summary table with the status of each sample of the experiment**

![alt text](https://github.com/INMEGEN/ARPA/blob/main/images/web-summary-table.jpeg?raw=true)


**It also allow the visual inspection of the amplification curves per sample**

![alt text](https://github.com/INMEGEN/ARPA/blob/main/images/web-sample-curves.jpeg?raw=true)


**Finally, the user can visualize the QC summary table and amplification curves**

![alt text](https://github.com/INMEGEN/ARPA/blob/main/images/web-qc-table.jpeg?raw=true)

![alt text](https://github.com/INMEGEN/ARPA/blob/main/images/web-qc-curves.jpeg?raw=true)




### AUTOMATIC GENERATION OF HTML REPORTS 

**Individual report (in HTML format) per sample is generated. It contains the amplificacion curves and QC results of the plate.**

![alt text](https://github.com/INMEGEN/ARPA/blob/main/images/html_sample.png?raw=true)

**A quality HTML report with the quality assesment and the control amplification curves is generated**

![alt text](https://github.com/INMEGEN/ARPA/blob/main/images/html_qc.png?raw=true)

## INSTALLATION

 - Download the repository

```
git clone https://github.com/guillermodeandajauregui/ARPA.git
```
You could also download the ZIP file from the download menu in the repository and uncompress the folder


### OPTION 1

 - Move to the repository directory

```
cd ARPA
```

 - Run the installation script. This script will install any required dependencies

```
sh install.sh
```
You will be prompted to enter your user password as some commands will do installations as root

 - Run the launcher script

```
sh launcher.R
```

### OPTION 2

 - Open RStudio
 - Open the app.R file in RStudio
 - Click the Run button in Rstudio

## TEST DATA

A sample EDS file and a sample report are provided in the sample_files directory
 
## AUTHORS

 - Laura Gómez-Romero [lgomez@inmegen.gob.mx]
 - Guillermo de Anda Jauregui [gdeanda@inmegen.edu.mx ]
 - Hugo Tovar [hatovar@inmegen.gob.mx] 
