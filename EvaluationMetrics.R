# EVALUATION METRICS INFORMATION RETRIEVAL
# Translated and adapted accordingly from Python to R
# Original implementation in Python by bwhite (https://gist.github.com/bwhite/3726239)
# Created in R by Sofia Yfantidou (https://github.com/syfantid)

# 1. DISCOUNTED COMULATIVE GAIN
# Args:
# r: Relevance scores (vector with length >= k and length > 1) in rank order (first element is the first item) (Levels: 0-5)
# k: Number of results to consider
# method: 
# If 0 then weights are [1.0, 1.0, 0.6309, 0.5, 0.4307, ...]
# If 1 then weights are [1.0, 0.6309, 0.5, 0.4307, ...]
# Returns: Discounted cumulative gain
dcg_at_k <- function(r, k, method=0) {
  # Check invalid input arguments 
  if(length(r) <= 1 || length(r) < k) {
    stop("Please provide valid arguments. The vector's length must be greater than 1 and greather than or equal to k.")
  }
  # Adaptations to prevent numeric errors
  if(k > length(r)) {
    k <- length(r);
  } else if (k < 2) {
    r[2:length(r)] = 0
    k <- 2
  }
  # The actual DCG
  r <- r[1:k]
  dcg <- 0
  if(length(r) != 0) {
    if(method == 0) {
      dcg <- r[1] + sum(r[2:length(r)] / log2(2 : length(r)))
    } else if (method == 1) {
      dcg <- sum(r / log2(2 : (length(r) + 1)))
    } else {
      stop("Invalid method code. Please try again.")
    }
  }
  return(dcg)
}
# Example from
# http://www.stanford.edu/class/cs276/handouts/EvaluationNew-handout-6-per.pdf
# r <- c(3, 2, 3, 0, 0, 1, 2, 2, 3, 0)
# dcg_at_k(r, 1) 
# 3
# dcg_at_k(r, 1, 1) 
# 3
# dcg_at_k(r, 2)
# 5
# dcg_at_k(r, 2, 1)
# 4.26186
# dcg_at_k(r, 10)
# 9.605118
# dcg_at_k(r, 10, 1)
# 8.318753
# dcg_at_k(r,11)
# 9.605118

##################################################################################################

# 2. NORMALIZED DISCOUNTED COMULATIVE GAIN
# Args:
# r: Relevance scores (vector with length >= k and length > 1) in rank order (Levels: 0-5)
# (first element is the first item)
# k: Number of results to consider
# method: 
# If 0 then weights are [1.0, 1.0, 0.6309, 0.5, 0.4307, ...]
# If 1 then weights are [1.0, 0.6309, 0.5, 0.4307, ...]
# Returns:
# Normalized discounted cumulative gain
ndcg_at_k <- function(r, k, method=0) {
  dcg_max = dcg_at_k(sort(r,decreasing = TRUE), k, method)
  if(dcg_max == 0) {
    return(0)
  }
  ndcg <- dcg_at_k(r,k,method) / dcg_max
  return(ndcg)
}
# Example from
# http://www.stanford.edu/class/cs276/handouts/EvaluationNew-handout-6-per.pdf
# r = c(3, 2, 3, 0, 0, 1, 2, 2, 3, 0)
# ndcg_at_k(r, 1)
# 1.0
# r = c(2, 1, 2, 0)
# ndcg_at_k(r, 4)
# 0.9203032077642922
# ndcg_at_k(r, 4, method=1)
# 0.96519546960144276

##################################################################################################

# 3. PRECISION @ K
# Args:
# r: Relevance scores (vector with length >= k >= 1 and length > 1) in rank order (first element is the first item) (Levels: 0-1)
# Returns: Precision @ k
precision_at_k <- function(r, k) {
  # Check invalid input arguments 
  if(length(r) <= 1 || length(r) < k) {
    stop("Please provide valid arguments. The vector's length must be greater than 1 and greather than or equal to k.")
  }
  # Adaptations to prevent numeric errors
  if(k > length(r)) {
    k <- length(r);
  } else if (k < 2) {
    r[2:length(r)] = 0
    k <- 2
  }
  r = r[1:k] 
  precision <- mean(r)
  return(precision)
}
# Example
# r = c(0,0,1)
# precision_at_k(r,1)
# 0
# precision_at_k(r, 2)
# 0
# precision_at_k(r, 3)
# 0.33333333
# precision_at_k(r, 4)
# Error in precision_at_k(r, 4) : Please provide valid arguments. The vector's length must be greater than 1 and greather than or equal to k.

##################################################################################################

# 4. AVERAGE PRECISION
# Args:
# r: Relevance scores (vector with length >= k >= 1 and length > 1) in rank order (first element is the first item) (Levels: 0-1)
# Returns: Average precision
average_precision <- function(r) {
  out = list()
  for(k in (1:(length(r)-1))) {
    out[k] = precision_at_k(r,k+1)
  }
  return(mean(unname(unlist(out))))
}
# Example
# r = c(1, 1, 0, 1, 0, 1, 0, 0, 0, 1)
# average_precision(r)
# 0.6332451


##################################################################################################

# 5. MEAN AVERAGE PRECISION
# Args:
# rs: Iterator (list) of relevance scores in rank order (first element is the first item) (Levels: 0-1)
# Returns: Mean average precision
mean_average_precision <- function(rs) {
  map <- list()
  for(i in 1:(length(rs[,1]))) {
    r <- as.numeric(as.vector(unname(unlist(rs[i,]))))
    map[i] = average_precision(r)
  }
  map <- unname(unlist(map))
  return(mean(map))
}


##################################################################################################

# 6. BREEZE's R-SCORE UTILITY
# Args:
# r: Relevance scores in rank order (first element is the first item) (Levels: 0-5)
# Returns: Breeze's R-score utility
breeze_utility <- function(r, a=1.5, d=2) {
  if(a == 1) {
    stop("Please provide a value for a, which is different than 1.")
  }
  r <- as.numeric(as.vector(unname(unlist(r))))
  breeze <- 0
  for(i in 1:length(r)) {
    breeze <- breeze + max(0,(r[i]-d))/(2^((i-1)/(a-1)))
  }
  return(breeze)
}


