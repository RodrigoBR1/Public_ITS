**Data repository for "**International Technological Subordination: The Geopolitical Economy of Circuits of Extraction. "****

Hi,

I hope you are doing well. At the moment, this repository ("repo") is curated solely by me (Rodrigo), so for any inquires about the data and its use please feel free to reach me. My current work e-mail is [rodrigo.brito@mail.udp.cl](rodrigo.brito@mail.udp.cl), please state "ITS repository" in the subject so I can answer as soon as possible in case of an issue related to data quality..

This repo contains a document called "Appendix B" containing a description of how the ITS dataset was built, and two folders:

1. **data,** which contains **the final dataset** compiled by the authors for the research paper;
2. **plots**, which contains various plots made to create and explore the dataset, as well as for testing the International Technological Subordination (ITS) theoretical framework. The *plots* folder contains 5 subfolders:
   1. **index-v1**, containing plots used to test the possibility of indexing the three main indicators analyzed ([a] Technology Import Dependency Ratio (TIDR); [b] Count of relevant patents  and; [c] net stock of Gross Fixed Capital Formation (GFCF). After consultation with [this 2008 OECD manual](https://doi.org/10.1787/9789264043466-en), the research team decided that such an indexation wouldn't be statistically funded and therefore didn't use that data nor the plots in the paper. Nevertheless, we the authors would like to encourage the search for such new metrics to analyze development in a global perspective, and therefore still include the results of our efforts in both the data and the plots of this repo.
   2. **plots-v1**, contains the first version of TIDR plots, in general and related to USA or China trade, for all countries and years available, with lines across y = 50 | -100 | -400 to mark interesting thresholds in the data. A plot for GFCF is made, also for all countries and years available, with lines at y = 100 | -100 to mark interesting thresholds again. Patent ownership data is ploted separately (count of agriculture patents, then mining patents, then data processing patents) and unified into a single count of patents. A correlation test is made between ECI and TIDR is made (but finally discarded), as well as a summary table with top 5 and bottom 5 countries for each indicator of interest.
   3. **plots-v2**, contains basically the same structure of the plots in v1, but more detailed (for example, there are plots for Latin American (LAS) countries and top 10 or *most dominant* countries in the Global Economy). In this step of the data process, efforts were made for a last minute idea to search for a proxy to try picture the near future of the ITS dynamics in the world. This idea was quickly discarded, test can be found in the *plots-v2.r* file
   4. **plots-v3**, contains the final plots used (and some unused) for the LAS analysis in the paper.
   5. **pre-plots**, contains some miscellaneous test run with the objective of finding/displaying tendencies or patterns in the data. Efforts to plot some data as *.gif* were made, but discarded.

