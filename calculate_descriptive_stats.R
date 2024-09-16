load("hw2_list.rda")

calculate_stats <- function(bp_list, stat = "mean") {
    #get `stat` param from function
    stat_arg <- get(stat)

    #initialize return list
    sumstats <- list()

    # get names
    for (i in names(bp_list)) {
        df <- bp_list[[i]]
        by_trt_stats <-c()

        # get numeric columns
        num_vars <- names(df)[sapply(df, is.numeric)]

        # calculate stats for numeric variables
        for (j in num_vars) {
            col_val <- stat_arg(df[[j]])
            stat_name <- paste(stat, j, sep = "_")
            # col_label <- toTitleCase(gsub("_", " ", stat_name))
            by_trt_stats[stat_name] <- col_val
            # by_trt_labels[stat_name] <- col_label
        }

        # store vector in the list by group name
        sumstats[[i]] <- as.data.frame(t(by_trt_stats))

        #split column names and combine with $
        colnames(sumstats[[i]]) <- colnames(sumstats[[i]]) <- sapply(colnames(sumstats[[i]]), function(x) {
            paste(strsplit((gsub("_", " ", x)), " ")[[1]], collapse = " ")
        })
    }

    # return list of data frames
    return(sumstats)
}

tst_mean_def<-calculate_stats(bp_list)
tst_mean_named<-calculate_stats(bp_list,"mean")
tst_sd<-calculate_stats(bp_list,"sd")
tst_min<-calculate_stats(bp_list,"min")
tst_max<-calculate_stats(bp_list,"max")
tst_var<-calculate_stats(bp_list,"var")


#Manual checks
# bp_list_chk <- bp_list[["treatment"]]
# mean(bp_list_chk$pre_bp)
# mean(bp_list_chk$post_bp)
# mean(bp_list_chk$diff_bp)
#
# bp_list_chk <- bp_list[["placebo"]]
# mean(bp_list_chk$pre_bp)
# mean(bp_list_chk$post_bp)
# mean(bp_list_chk$diff_bp)
```
