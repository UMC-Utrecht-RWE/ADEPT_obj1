# Repo template
## Template Description
This repository presents the structure for every new RWE project developed in the RWE Research Team at UMC Utrecht.

The folder structure of the repository is based on the RWE Pipeline (see figure below).

![RWE-PIPELINE](https://github.com/UMC-Utrecht-RWE/repo_template/blob/main/docs/RWE-PipelineDiagram.jpg)


The pipeline is composed by Transformation steps (T) and datasets (D) and folders for common functionality and configuration files. Between each Transformation step there are a set of dataset - wihin the folder D - provided by the previous step. The starting D folder is the D2 and it is provided by a Data Access Provider (DAP). 

Each Transformation step is composed by 3 other subfolders:
- configuration: configuration files (metadata) that can be used for automatization of the script
- source_code: script steps used within the transformation step
- intermediate_data_files: intermediate data sets used within the transformation step

## Use GNU GPL3.0
The GNU GENERAL PUBLIC LICENSE is the license to be used by default for the RWE projects unless there is a license that better fits the requirement of the study.

## New repo readme template:

### Study name
=============
- Study type: TBD
- Tags: TBD
- Study lead: TBD
- Study contributors: TBD
- Study start date: TBD
- Study end date: TBD
- Publications: TBD
- Contact: TBD
  
### Objectives:
Add details about the study objectives

### How to run:

Add details about how to execute the script

