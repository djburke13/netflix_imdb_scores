<h1> Netflix IMDB Score Analysis, Prediction & Classification</h1>

📖 Titles.csv https://www.kaggle.com/datasets/victorsoeiro/netflix-tv-shows-and-movies/data?select=titles.csv 

🎥 Credits.csv: https://www.kaggle.com/datasets/victorsoeiro/netflix-tv-shows-and-movies/data?select=credits.csv 

Merging:
In order to meaningfully merge the above 2 datasets, the key feature that we will merge on will be the Movie or Show ID column for titles.csv and the Title ID for the credits.csv. This column contains the unique ID for the TV show or movie on JustWatch. 
We will also continue to use this column as the unique ID column since the other columns contain repeats. A data merging issue we have is that each row in the credits.csv data set is an actor, and we want each row of our analysis dataset (and each row in titles.csv) to be for movies. 
We will group by Title ID and create a list of actors and directors per movie. Other than that issue, since there are no missing values for the movie id attribute in either dataset, there should be no challenges when merging.


Analysis: 
In terms of the analysis part of our project, we aim to analyze movie ratings in respect to the other attributes (actors, directors, description, runtime) to detect clusters and relative importance. We hope to answer questions such as: “Does the presence of XYZ actor tend to give their movies a higher overall average rating?”, “Does runtime or genre affect the IMDB rating more?” and “What are the similarities in actors and age certification in the highest performing cluster of movies?”. This will inform our data cleaning and wrangling decisions in these ways. 


🧹 Cleaning: 
If the IMDB rating, the TMDB popularity, and the TMDB score, we will drop the row, as we are using these attributes to measure popularity
Wrangling: We will be dropping some columns from titles.csv that are extraneous, like IMDB votes or IMDB ID (since we already have a unique ID variable). 
Wrangling: We may derive attributes from the credits.csv dataset such as the number of directors that worked on a project (Is multiple directors working on a project correlated with a higher rating?)

Results:
Through feature engineering, feature selection, data analysis, and classification trees, we have analyzed how different factors play a role in classifying IMDB scores of movies and tv-shows: 

⚡ My best performing model indicates that the 3 most informative features in reducing impurity for IMDB classification of tv-shows are **runtime, release year, and actor median**, where the first/most informative feature is runtime. 

⚡ The identifying features for movies are **genres, actor median, and runtime**, where the first/most informative feature is genre. We have proven our hypothesis that since shows and movies are different in nature, they will have different factors that influence their IMDB score (even though they come from the same dataset with the same features). 
