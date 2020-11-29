# Vegetation_Classification
Scripts for Building or Assessing the Vegetation Classification of BEC

The flow of the scripts is 
0. Import data from Vpro and create optional roll-ups for analyses (species lumping)

Optionally : generate some new working units by cluster analysis if no site unit classification has been developed

1 Generate summary vegetation values from submitted site unit tables

2 Evaluation of site series and plant association concepts based on sample intensity, vegetation and site consistency

3 Build Class and Order level machine learning model and predict membership of existing site series, newly submitted site units or unclassified plots

4 Pair-wise analysis of site units for consolidation into subassociations or associations within SubOrders or submitted SU table.

5 Generate summary statistics and spatial distribution for chosen units (vegetation, environment (aSMR/SNR), climate)

6 Noise Clustering to place new plots into existing hierarchical units

7. Alliance analysis using indicator groups of site series within suborders

Some general functions:

Generate field gudie formated summary vegetation tables from submitted 

Build data.tree function for managing hierarchy

Build Postgres database structure to mirror and update ACCESS tables



