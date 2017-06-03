# Search-Engine-Evaluation-Metrics
Information Retrieval Systems (e.g. Search Engines) evaluation metrics in R. 

Translated and adapted accordingly from Python to R. Original implementation in Python by bwhite (https://gist.github.com/bwhite/3726239). Translated in R by Sofia Yfantidou (https://github.com/syfantid).

Metrics included:
* Precision@K : The top k results need to be examined to determine if they are relevant or not.
* Average Precision 
* Mean Average Precision : Mean average precision for a set of queries is the mean of the average precision scores for each query.
* Discounted Comulative Gain (DCG) : DCG uses a graded relevance scale of documents from the result set to evaluate the usefulness, or gain, of a document based on its position in the result list. The premise of DCG is that highly relevant documents appearing lower in a search result list should be penalized as the graded relevance value is reduced logarithmically proportional to the position of the result.
* Normalized Discounted Comulative Gain (NDCG) : The normalised version of DCG uses an ideal DCG. To this end, it sorts documents of a result list by relevance, producing an ideal DCG at position p, which normalizes the score.
* Breeze's R-Score Utility : Calculates a utility score for the results' list of a search query. The premise of this metric is that a recommendation's value is decreased exponentially based on its position in the retrieved recommendation list.
