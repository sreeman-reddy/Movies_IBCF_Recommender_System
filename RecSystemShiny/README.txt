This folder contains the shiny app that runs the recommender system. 

It is powered by the datasets previously used in the initial analysis, and also two other files that were created during the initial analysis:

smat: This matrix initially denoted the 3706-by-3706 cosine similarity matrix among the 3706 movies. It was further modified in the following way: For each row, the non-NA similarity measures were sorted and the top 30 measures were retained while the rest were set to NA. The final similarity matrix is not symmetric.

popular50: This is a csv containing the MovieID and Name of the 50 most popular movies based on average user rating in the dataset.


The Shiny app uses the IBCF model constructed during the initial analysis. 
