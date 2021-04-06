# Elections Vote Counting
# 6 Apr 2021
# Bernard Boey (bernard@u.yale-nus.edu.sg)
# Source code available on: https://github.com/bernardboey/stugov-vote-counting



#### INSTRUCTIONS ####
# To run this file, edit the "MANUAL INPUT" section first.
# Then, click on "Source".
# Look at the output text in the console for the results.



#### MANUAL INPUT ####

# Type in file name of the csv file
original_df <- read.csv("test.csv", stringsAsFactors = FALSE)

# Get the total number of eligible voters from Registry and change
eligible_voters <- 970  

# Decide the quorum level and change
quorum <- 15/100

# Write down the column numbers of all the columns containing votes
# The names of those columns in the csv file should be the names of the positions
position_columns <- c(8, 10, 12, 14, 16, 18)

# Set to TRUE to print out debugging info for tie breaking
debug <- FALSE



#### CLEANING ####

df <- original_df[, position_columns]
positions <- names(df)



#### QUORUM ####

# Calculating Quorum
total_voters <- nrow(df)
threshold <- ceiling(quorum * eligible_voters)
if (total_voters >= threshold) {
  cat("Total votes were " , total_voters, " and we reached quorum!\n\n", sep = "")
} else {
  cat("Total votes were ", total_voters, " and we did not reach quorum.\n\n", sep = "")
}



#### FUNCTIONS ####

# Input CSV file should be in this format. A, B, C are the candidates for President.
# Notice that it is possible for some ranks to be left blank (see 3rd row)
# and for voters to list 2 candidates as the same rank (see 4th row).
# These errors will be dealt with later on.

# |   President    | Vice President | ..... |
# |-----------------------------------------|
# | A=>2,B=>1,C=>3 | .............. | ..... |
# | A=>3,B=>1,C=>2 | .............. | ..... |
# | A=>1,B=>,C=>2  | .............. | ..... |
# | A=>1,B=>2,C=>2 | .............. | ..... |
# | A=>3,B=>2,C=>1 | .............. | ..... |


# For each contested position, we create a ranks_df.
# ranks_df is a dataframe where each row represents a voter.
# There are `n` columns where `n` is the number of candidates for that position,
# Each column `i` contains the candidate that the voter ranked in the `i`th position.
# There is also a current_rank column that represents which rank is currently being considered.
# (it is initialised to 1 because all the voter's 1st choices are considered first)
# There is also a current_candidate column that represents which candidate corresponds to the current_rank.

# E.g. The ranks_df for President (see example above) looks like this:

# |  1  |  2  |  3  | current_rank | current_candidate |
# |----------------------------------------------------|
# |  B  |  A  |  C  |       1      |         B         |
# |  B  |  C  |  A  |       1      |         B         |
# |  A  |  C  |  NA |       1      |         A         |
# |  A  |  NA |  NA |       1      |         A         |
# |  C  |  B  |  A  |       1      |         C         |

# Notice how there are NA values:
# For the 3rd row, the voter didn't choose anyone as their 3rd rank, so it is NA.
# For the 4th row, the voter chose both B and C as their 2nd rank, so those votes are nullified.

# generate_ranks_df is a function that generates and returns the ranks_df
# list_of_raw_votes is basically the corresponding column from the csv file (excluding header)
generate_ranks_df <- function(list_of_raw_votes) {
  num_candidates <- length(unlist(strsplit(list_of_raw_votes[1], ",")))
  ranks_df <- data.frame(matrix(ncol = num_candidates, nrow = 0), stringsAsFactors = FALSE)
  colnames(ranks_df) <- 1:num_candidates  # Rename column names as [1, 2, 3, ...]
  for(raw_vote in list_of_raw_votes) {  # E.g. raw_vote = "A=>2,B=>1,C=>3"
    ranks_df <- parse_and_add_ranking(raw_vote, ranks_df)
  }
  # Intialise current_rank as 1
  ranks_df$current_rank <- 1
  # Intialise the current_candidate column
  ranks_df$current_candidate <- ranks_df[, 1]
  return(ranks_df)
}


# parse_and_add_ranking parses a raw_vote (e.g. "A=>2,B=>1,C=>3") and
# adds the rankings to the ranks_df (i.e. adding a row to ranks_df)
parse_and_add_ranking <- function(raw_vote, ranks_df) {
  # Split based on ","
  # "A=>2,B=>1,C=>3" becomes ["A=>2", "B=>1", "C=>3"]
  list_of_rankings <- unlist(strsplit(raw_vote, ","))
  num_candidates <- length(list_of_rankings)
  
  # Intialise the new row with NAs
  ranks_df[nrow(ranks_df) + 1,] <- rep(NA, length(list_of_rankings))
  
  invalid_ranks <- c()  # Intialise list of invalid_ranks
  
  # Iterate over the list of rankings
  for(text in list_of_rankings) {
    # Split based on "=>"
    # "A=>2" becomes ["A", "2"]
    candidate_and_rank <- unlist(strsplit(text, "=>"))
    candidate <- candidate_and_rank[1]
    rank <- candidate_and_rank[2]
    
    # If rank is NA, that means voter did not rank that candidate.
    if(is.na(rank)) {
      next  # Just skip because already initialised to NA
    }
    
    # Check if voter already chose another candidate for the same rank
    if(!is.na(ranks_df[nrow(ranks_df), rank])) {
      # If yes, the ranking will be invalid. We keep track by adding it to the list of invalid ranks
      # We don't do anything else at this point (the candidate will still be added, thereby overwriting the other candidate)
      # We will need to fix this later by changing them to NA
      invalid_ranks <- append(invalid_ranks, rank)
    }
    
    # Add in the candidate for the corresponding rank
    ranks_df[nrow(ranks_df), rank] <- candidate
  }
  
  # Change all invalid ranks to NA
  for(rank in invalid_ranks) {
    ranks_df[nrow(ranks_df), rank] <- NA
  }
  
  # If there are NA ranks, find which is the lowest NA rank, and change all subsequent ranks to NA
  # This is because once a voter's rank is NA, their entire ballot is exhausted and will not count
  NA_ranks <- which(is.na(ranks_df[nrow(ranks_df), 1:num_candidates]))
  if(length(NA_ranks) >= 1) {
    for(rank in min(NA_ranks):num_candidates) {
      ranks_df[nrow(ranks_df), rank] <- NA
    }
  }
  
  return(ranks_df)
}


# vote_table is simply a table detailing how many votes each candidate gets

# Takes the vote_table and print the distribution of votes
# (vote_table is assumed to be sorted in decreasing order)
print_vote_distribution <- function(vote_table, ranks_df) {
  for(i in 1:length(vote_table)) {
    cat(names(vote_table[i]), ": ", vote_table[[i]], " votes\n", sep = "")
  }
  # Print invalid votes for that round
  cat("(Invalid: ", sum(is.na(ranks_df$current_candidate)), " votes)\n", sep = "")
}


# This function finds the lowest candidate, so that they can be eliminated.
# This function also helps to tie break in the event that there are multiple
# candidates with the same lowest number of votes.
# Process for tie-breaking: We check the rank 1 votes and see which of the tied candidates
# received the lowest. If still tie, we check the rank 2 votes, etc. so on and on.
find_lowest_candidate <- function(vote_table, ranks_df) {
  # Get the names of candidates which have the lowest number of votes.
  # Might be just one candidate, or might be multiple (in the event of a tie)
  lowest_candidates <- names(which(vote_table == min(vote_table)))

  # If there is only one lowest candidate, then we just return that candidate's name. End of function.
  if(length(lowest_candidates) == 1) {
    return(lowest_candidates)
  }
  
  # If there are more than one lowest candidate, need to tie break.
  # `i` keeps track of which rank we are using to help tie break.
  i <- 1
  
  while(length(lowest_candidates) > 1) {
    # Check if we have exhausted all ranks for the tie break
    # If yes, that means that it was a tie between two candidates for each of the ranks (rank 1, rank 2, rank 3, etc..)
    # In such a case, we cannot resolve this tie break. Return NA.
    if(i > ncol(ranks_df) - 2) {
      return(NA)
    }
    
    if(debug) {
      cat("\nDEBUG: Current lowest candidates are: ", paste(lowest_candidates, collapse = ", "), "\n", sep = "")
    }
    
    # Get the vote distribution for rank `i` votes between the currently tied candidates
    votes_among_lowest_candidates <- table(factor(ranks_df[ranks_df[, i] %in% lowest_candidates, i], levels = lowest_candidates))
    # Try to use that to tie break. We get the names of candidates which have the lowest number of votes again.
    lowest_candidates <- names(which(votes_among_lowest_candidates == min(votes_among_lowest_candidates)))
    
    if(debug) {
      cat("DEBUG: Vote distribution\n")
      cat(names(votes_among_lowest_candidates), "\n")
      cat(votes_among_lowest_candidates, "\n")
    }
    
    i <- i + 1
    
    # If tie has been broken (i.e. lowest_candidates is just 1 person), then we exit the while loop.
    # If tie has not been broken (i.e. lowest_candidates is 2 or more ppl), then we continue the while loop
  }
  
  # Print out info on the tie break as well as which rank's votes were used to break the tie.
  cat("\n", lowest_candidates, " is eliminated based on tie breaking rules (received lowest number of rank ", i - 1, " votes).", sep = "")
  
  # If we did not use rank 1 to break the tie, that means that the candidates were also tied in rank 1.
  # Print out explanation that the candidates were still tied, so we needed to use a lower rank (e.g. rank 2 or lower) to break the tie
  if(i > 2) {
    cat("\n(The candidates were also tied in votes for rank ", paste(1:(i-2), collapse = ", "), ")\n", sep = "")
  }
  
  return(lowest_candidates)
}


# Redistributes votes by editing the current_rank and current_candidate columns
# `eliminated` is a list of eliminated candidates
redistribute_votes <- function(ranks_df, eliminated) {
  # Iterate through every row of ranks_df
  for(i in 1:nrow(ranks_df)) {
    # If the current_candidate has already been eliminated
    while(ranks_df$current_candidate[i] %in% eliminated) {
      # Then we need to redistribute the vote
      # So we go to the next rank (increment current_rank by 1)
      ranks_df$current_rank[i] <- ranks_df$current_rank[i] + 1
      # Then, we find which candidate corresponds to the incremented rank
      ranks_df$current_candidate[i] <- ranks_df[i, ranks_df$current_rank[i]]
    }
    # Note: It is a while loop rather than an if statement because the
    # redistributed candidate might also have been previously eliminated.
    # So we need to keep checking if the current_candidate has been eliminated
    # and redistribute accordingly.
  }
  return(ranks_df)
}

# E.g. We make use of the same example as above (ranks_df for the President position).
# After redistributing C's votes, this is how it should look like:

# |  1  |  2  |  3  | current_rank | current_candidate |
# |----------------------------------------------------|
# |  B  |  A  |  C  |       1      |         B         |
# |  B  |  C  |  A  |       1      |         B         |
# |  A  |  C  |  NA |       1      |         A         |
# |  A  |  NA |  NA |       1      |         A         |
# |  C  |  B  |  A  |       2      |         B         |



# This is the main function that counts votes
# list_of_raw_votes is basically the corresponding column from the csv file (excluding header)
# position is the name of the position that is being contested
count_votes <- function(list_of_raw_votes, position) {
  cat("\nVote counting for ", position, ":\n\n", sep = "")
  
  # Get list of all candidates
  candidates <- unlist(lapply(strsplit(list_of_raw_votes[1], ","), gsub, pattern = "=>.*", replacement = ""))
  
  # Generate ranks_df
  ranks_df <- generate_ranks_df(list_of_raw_votes)
  
  # Initialise list of eliminated candidates
  eliminated <- c()
  
  # Iterate through each round of elimination and redistribution
  while(TRUE) {
    # Count how many votes there are for each candidate in this round
    vote_table <- sort(table(factor(ranks_df$current_candidate, levels = candidates)), decreasing = TRUE)
    
    print_vote_distribution(vote_table, ranks_df) 
    
    # Check if we have a winner (i.e. if any candidate has majority of votes)
    total_votes <- sum(table(ranks_df$current_candidate))
    if (any(vote_table > total_votes / 2)) {
      winner <- names(which(vote_table > total_votes / 2))
      cat("\nThe winner for ", position, " is ", winner, ".\n\n", sep = "")
      break
    }
    
    # Find the lowest candidate
    lowest_candidate <- find_lowest_candidate(vote_table, ranks_df)
    # If NA, that means there are tied candidates and it is impossible to tie break
    if(is.na(lowest_candidate)) {
      cat("\nNo winner for ", position, " (unable to tiebreak).\n\n", sep = "")
      break
    }
    
    # Add lowest candidate to list of eliminated candidates
    eliminated <- append(eliminated, lowest_candidate)
    # Remove lowest candidate from list of active candidates
    candidates <- candidates[candidates != lowest_candidate]
    
    # Redistribute votes from lowest candidate + eliminated candidates to other candidates
    ranks_df <- redistribute_votes(ranks_df, eliminated)
    cat("\nAfter redistributing votes from ", lowest_candidate, " as per IRV procedures:\n\n", sep = "")
  }
}


#### VOTE COUNTING ####

for(i in 1:length(positions)) {
  position <- positions[i]
  list_of_raw_votes <- df[, i]
  count_votes(list_of_raw_votes, position)
}