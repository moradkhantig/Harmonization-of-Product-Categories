# Harmonization of Product Categories
##  Problem Statement
The Department of Toxic Substances Control's Safer Consumer Products Program (SCP) is concerned with adverse impacts from exposures to hazardous chemicals that function as ingredients in consumer products. In evaluating exposures to public health and the environment, SCP considers many factors such as:
  1.  the potential for chemicals in consumer products to adversely impact children's and workers' health
  2.  the potential for chemicals in consumer products to adversely impact environmental justice communities
  3.  the prevalence of chemicals in consumer products

If SCP determines that a specific chemicas or class of hazardous chemicals in consumer products has the potential to cause widespread adverse impacts to people or the environment, it then proposes and adopts through formal rulemaking a Priority Product. A Priority Product is a product-chemical combination which can have adverse impacts on public health or the environment. Once a Priority Product is adopted, manufacturers are then encouraged to perform alternative analyses to determine if there is a safer alternative to the chemical or class of chemicals identified in the Priority Product regulation. Based on the alternative analyses submitted by manufacturers, SCP issues a regulatory response such as requiring information to be provided to consumers or requiring the implementation of safety measures and engineering controls.

During product evaluation, SCP studies the whole product lifecycle starting from the manufacturing stage to a product's end-of-life and disposal stage to understand how the chemicals in a product may adversely impact people or the environment. SCP also consults various product and marketing databases to gain an understanding of product types, marketing trends, and determine the number and prevalence of chemicals used in a consumer product. However, one issue with product databases is that products may be categorized differently in each database; therefore it is difficult to compare data from different product databases. Currently, when faced with this daunting task, SCP tries to manually harmonizes product categories; however, such a task can be cumbersome and may impact a project's timeline and resources beyond those anticipated during project planning. Here, we propose to use machine learning as method to harmonize product categories of data obtained from different product databases. For this project we made use of a publicly available and easy to download product database, the United States Environmental Protection's Chemical and Product Database (CPDat) to find an algorithm which may be used as a product classification harmonizer. More information about the CPDat database can be found in the report titled Harmonization of Product Categories.

This repository includes the following:
  1.  a Raw Data folder containing the downloaded CPDat database. Note that the CPDat database contains separate excel spreadsheets and so the CPCat download has its own folder.
  2.  a Cleaned Data folder containing R scripts used to clean and explore the CPDat dataset along with cleaned versions of the databaset, including results from model training.
  3.  a Data Visualization folder containing visuals created from exploration of the dataset 
  4.  an Rmd file of the report titled Harmonization of Product Categories
  5.  a pdf file of the Harmonization of Product Categories
  6.  an R script used for machine learning
  7.  an R script which further analyzes the performance of the trained models
  8.  Excel files of the CPDat dataset that was imported for model training
  9.  supplementary data files which are part of the pdf report

##  Conclusion
We found that the k-nearest neighbor algorithm was the best performing method for classification of products in CPDat. Additional research is needed to better understand if the k-nearest neighbor algorithm can be used as a product classification harmomizer.  

