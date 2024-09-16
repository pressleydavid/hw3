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
            col_stat <- stat_arg(df[[j]])
            stat_name <- paste(stat, j, sep = "_")
            by_trt_stats[stat_name] <- col_stat
        }
        
        # store vector in the list by group name
        sumstats[[i]] <- as.data.frame(t(by_trt_stats))
    }
    
    # return list of data frames
    return(sumstats)
}
tst_mean_def<-calculate_stats(bp_list)

bp_list_chk <- bp_list[["treatment"]]
mean(bp_list_chk$pre_bp)
mean(bp_list_chk$post_bp)
mean(bp_list_chk$diff_bp)

bp_list_chk <- bp_list[["placebo"]]
mean(bp_list_chk$pre_bp)
mean(bp_list_chk$post_bp)
mean(bp_list_chk$diff_bp)