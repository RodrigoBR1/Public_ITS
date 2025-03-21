# **Repository for "**International Technological Subordination: An Assessment of Global Power in Resource-Based Industries** **"****

## Introduction

Hi,

I hope you are doing well. At the moment, this repository ("repo") is curated solely by me (Rodrigo), so for any inquires about the data and its use feel free to reach me. My current work e-mail is [rodrigo.brito@mail.udp.cl](rodrigo.brito@mail.udp.cl), please state "ITS repository" in the subject so I can answer as soon as possible in case of an issue related to data quality.

## Contents

This repo contains a document called "DP-details.md" containing a more precise description of how the ITS dataset was built from a programming or computational perspective, and four folders:

1. **tech_ids**, which contains tables **hs_codes** and **cpc_codes**. The first contains Harmonized System (HS) codes and the latter contains Cooperative Patent Classification (CPC) codes; both are respectively used to identify technological commodities traded and intellectual property enforced in the interntional system, for the purpose of building the ITS dataset.
2. **data,** which contains the **final dataset** compiled by the authors for the research paper (called "ITS_v1.csv") and **sample datasets** containing raw data from the sources *UNCTADSTAT's COMTRADE* and *Google's Patents Publics Dataset*. For access to the complete UNCTADSTAT's COMTRADE dataset downloaded (containing detailed data on traded technologies of interest around the world), please contact the repo curator because its over 70GB of data to transfer. For access to Google Patents data, its easier to perform the data collection routine described in **DP-details** document.
3. **plots**, which contains various plots made to create and explore the dataset, as well as for testing the International Technological Subordination (ITS) theoretical framework, which may be of interest to understand the data. At the moment, the *plots* folder contains 2 subfolders:

   1. **selected10** contains timeseries plots of Counts of Internationalized Patents (PATS) and Technology Dependence Index (TDI) for a sample of key countries in technological development, in the categories of technologies identified through tables contained in folder **tech_ids.**
   2. **continental** contains timeseries plots of PATS and TDI similar as in prior folder, but for all countires available in the dataset and continentally identified through colors.
4. **dr-dp**, which contains key scripts to perform the computational routines necessary to retrieve and process data to build the ITS dataset.

## Further Research and Acknowledgments

This repository is intended to facilitate research on development theory (particularly about dependent development and international technological landscape) by providing a reproductible computational approach, that can surely be polished and further expanded. We the authors of the paper encourage researchers, policymakers, and analysts to explore the dataset and contribute to ongoing discussions in this field.

If you find this work useful for your research, please consider citing the paper or reaching out with feedback. Your insights are valuable for refining and expanding this dataset.

Thank you for your interest and engagement!
