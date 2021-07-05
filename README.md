# BEC_Hierarchy Vegetation Classification Analysis
Scripts for Building or Assessing the Vegetation Classification Hierarchy of BEC

The flow of the scripts is 
###0. Import data from Vpro and create optional roll-ups for analyses (species lumping)

Optionally : generate some new working units by cluster analysis if no site unit classification has been developed

1 Generate summary vegetation values from submitted site unit tables

2 Evaluation of site series and plant association concepts based on sample intensity, vegetation and site consistency

3 Build Class and Order level machine learning model and predict membership of existing site series, newly submitted site units or unclassified plots
Build a machine learning model of high-level hierarchical BEC Forest units
a. Create a ML model of Forested Orders using BECv11 site series that meet minimum plot requirements.
b. Use only tree species for first round of model build for the forested Orders.
c. Build hurdle model to separate treed vs non-treed units.
	May wish to assign a temporary notree pseudo species to all units that have no trees to help separate forested hierarchy from non-forested hierarchy (Hurdle model)
c. Predict membership of new BECv12 units and assign to orders then rebuild ML model.
d. Review any mis predicted site series for reassignment.
e. Compare similarity of tree composition within units (noise clustering) to find outliers that may represent new Orders (test by leaving out known units)
f. Create vegetation summaries of Orders and compare vegetation (all species now). Identify climatic indicator species groups.
g. Create climatic and site attributes summary of Orders 
h. Create maps of plot locations for each Order

4 Pair-wise analysis of site units for consolidation into subassociations or associations within SubOrders or submitted SU table.
 Do pair-wise analysis of site series within Orders using high constancy species to analyze site series and create Associations/SubAssociations.

5. Alliance analysis using indicator groups of site series within suborders
Build Alliances. Focus should be on identifying species groups that reflect different site conditions.
a. Use some of the techniques of indicspecies package to create Alliances based on indicator group creation and analysis.
b. Try to build a machine learning model of Alliances
c. Vegetation and Environment summary of Alliances


6 Document hierarchy
Generate summary statistics and spatial distribution for chosen units (vegetation, environment (aSMR/SNR), climate)

7. Build model to assign new units and unassigned plots
Challenge here is to make a constant list of species between model and new units.
a. Predict  and place new site series (draft BECv13) using ML . Noise Clustering to test for novel units that do not fit any existing.
b. Predict Order membership of BECMaster plots that are unassigned to existing site series and add to an Order_unplaced site unit under each Order.

8. Work toward model of non-forest classes.
Based on major site level differences rather than climate and using major species groups (e.g. Hydrophytic species)

Some general functions:

Generate field gudie formated summary vegetation tables from submitted 

Build data.tree function for managing hierarchy

Build Postgres database structure to mirror and update ACCESS tables



