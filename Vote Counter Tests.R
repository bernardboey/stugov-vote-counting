list_of_raw_votes_1 <- c("A=>1,B=>2,C=>3,D=>4,E=>5",
                         "A=>1,B=>3,C=>5,D=>2,E=>4",
                         "A=>1,B=>4,C=>2,D=>4,E=>5",
                         "A=>1,B=>4,C=>2,D=>3,E=>4",
                         "A=>1,B=>2,C=>2,D=>4,E=>4",
                         "A=>1,B=>,C=>,D=>,E=>",
                         "A=>1,B=>1,C=>3,D=>4,E=>5",
                         "A=>4,B=>,C=>1,D=>4,E=>5",
                         "A=>4,B=>2,C=>3,D=>1,E=>5",
                         "A=>,B=>2,C=>3,D=>4,E=>1")


ranks_df_1 <- data.frame("1" = c("A", "A", "A", "A", "A", "A", NA, "C", "D", "E"),
                         "2" = c("B", "D", "C", "C", NA, NA, NA, NA, "B", "B"),
                         "3" = c("C", "B", NA, "D", NA, NA, NA, NA, "C", "C"),
                         "4" = c("D", "E", NA, NA, NA, NA, NA, NA, "A", "D"),
                         "5" = c("E", "C", NA, NA, NA, NA, NA, NA, "E", NA),
                         "current_rank" = rep(1, 10),
                         "current_candidate" = c("A", "A", "A", "A", "A", "A", NA, "C", "D", "E"))
names(ranks_df_1) <- c(1, 2, 3, 4, 5, "current_rank", "current_candidate")

ranks_df_1_generated <- generate_ranks_df(list_of_raw_votes_1)
stopifnot(identical(ranks_df_1, ranks_df_1_generated))

count_votes(list_of_raw_votes_1, "Test Position")


vote_table_1 <- sort(table(factor(ranks_df_1$current_candidate, levels = c("A", "B", "C", "D", "E"))), decreasing = TRUE)
find_lowest_candidate(vote_table_1, ranks_df_1)

vote_table_1 <- sort(table(factor(ranks_df_1$current_candidate, levels = c("A", "C", "D", "E"))), decreasing = TRUE)
find_lowest_candidate(vote_table_1, ranks_df_1)


ranks_df_redistributed <- redistribute_votes(ranks_df_1, c("A"))