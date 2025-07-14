### Antiseizure meDication Exposure and Pregnancy and neonaTal outcomes research: ADEPT
=============
- Study type: Drug Utilisation
- Tags: TBD
- Study PI: Prof. Dr. Miriam Sturkenboom
- Study lead: Dr. Shahab Abtahi, Dr. Carlos E. Durán, Elizabeth Comberg
- Study programmer: Magdalena Gamba 
- Study contributors: TBD
- Study data sources: BIFAP (ES), SIDIAP (ES), FISABIO (ES), Val Padana (IT), CPRD (UK), NHR (NO), FHR (FI), EFEMERIS (FR), PHARMO (NL).
- Study start date: 2000-01-01 or first DEAP data availability
- Study end date: Last DEAP data availability
- Publications: TBD
- Contact: m.a.gamba@uu.nl
  
### Objectives:
The main objective of this study is to describe the utilisation of ASMs and related drugs (i.e., antiepileptics (ATC codes N03A), gabapentinoids (N02BF), and all benzodiazepines with antiepileptic properties) in pregnant women, other women of childbearing potential (12-55 years of age), and men (12 years and older). 

Sub-objectives: 

1. To estimate the annual incidence and prevalence rate of ASM use in women of childbearing potential and in men; 
2. To describe treatment duration, discontinuation, and treatment switches to other ASMs or alternative medications and polytherapy in women of childbearing potential and men; 
3. To estimate pre-pregnancy ASM use, and initiation and continuous use of ASMs during the pregnancy periodincidence and prevalence of specific ASM use in up to one year prior to and during the pregnancy period in all pregnant women; 
4. To estimate pre-pregnancy, early and late discontinuation of ASMs, treatment switches to other ASMs or alternative medications and polytherapy among pregnant women;
5. To estimate dose changes of ASMs in women prior to and during pregnancy. 

## 🚀 How to Run


### ⚙️ Requirements

- **R version:** This project was developed and tested with **R version 4.5.0** (please use this version or later to ensure compatibility).
  
- **CDM tables:** The scripts require the following CDM tables to be available in the specified CDM directory:
  - `CDM_SOURCE`
  - `EVENTS`
  - `INSTANCE`
  - `MEDICINES`
  - `METADATA`
  - `OBSERVATION_PERIODS`
  - `PERSONS`

---


### 1. 🛠 Installation  
1. Download the ZIP of the repository and extract its contents.  
2. Keep **only** the `RELEASE` folder — discard the `DEVELOPMENT` folder.

---

### 2. 📁 Setup  
Copy the `ADEPTOBJ1_scripts` folder from the `RELEASE` directory into the **same local directory** where you also store:
- the `CDMInstances` folder  
- the Level 1–3 check script folders

---

### 3. 🔧 Configuration (in `to_run.R`)

Open the `to_run.R` file located in the `ADEPTOBJ1_scripts` folder and follow the steps below:

#### 3.1 Select your DEAP source 
Uncomment the line corresponding to your DEAP data source.  

**Example for CPRD**:

```r
# DEAP_data <- "BIFAP"
DEAP_data <- "CPRD"  # ← example active line
# DEAP_data <- "EFEMERIS"
# DEAP_data <- "FIN_REG"
# DEAP_data <- "NOR_REG"
# DEAP_data <- "PHARMO"
# DEAP_data <- "SIDIAP"
# DEAP_data <- "VAL_PAD"
# DEAP_data <- "VID"
```

#### 3.2 Specify if you have data from one region or multiple regions (BIFAP) and set paths to CDM tables/regions  

**If using a single-region DEAP:**

```r
multiple_regions <- FALSE
CDM_dir <- "Path/To/Your/CDM/Folders/here" <- path to folder with all CDM tables 
```


**If using multiple regions (BIFAP):**

```r
multiple_regions     <- TRUE
multiple_regions_dir <- "Path/To/Your/Regional/Folders/Here/For/BIFAP" <- path to folder with regional folders, each containing CDM tables for that region
```

> ⚠️ **Important:** Only one of these options should be uncommented!

---

### 4. ✅ Run the Script  
Once the configuration is complete, run the `to_run.R` script.

---

### 5. 📤 Upload Output  
After the script finishes, upload the `D5_results` folder to myDRE. 
