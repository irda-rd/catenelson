#' @rdname cate_nelson
#' @export
#'   
cate_nelson_x <- function(x, y, n_group = 2, min_group = 2, min_crit = NULL, max_crit = NULL){
  #Order dataset by x
  i <- order(x)
  x <- x[i]
  y <- y[i]

  #Generate possible partitions and associated selection matrices
  n <- length(x)
  if(min_group < 2){
    stop("The mininum of different values per group must be at least 2 for x partitioning")
  }
  ##Generate possible divisions, with constraints
  division  <- group_division(x, n_group, min_group = min_group, min_crit = min_crit, max_crit = max_crit)
 
  ##Generate selection matrices from divisions
  group <- group_matrix(n, division)
  S <- group_selection_matrices(group)

  #Compute sum of squares (RSS = residual, E = explained, T = total) and R2
  rss <- cn_rss(S, y)
  tss <- cn_tss(y)
  ess <- tss - rss
  R2 <- 1 - rss/tss

  #Compute mid values associated to x divisions and join to df
  df <- data.frame(ess = ess, rss = rss, R2)
  crit_x_df <- as.data.frame(t(group_crit(x, division)), stringsAsFactors = FALSE)
  names(crit_x_df) <- paste0("crit_x_", seq_len(nrow(division)))
  df <- cbind(crit_x_df, df)

  #Ordering df and group in decreasing order of R2
  j <- order(df$R2, decreasing = TRUE)
  df <- df[j,]
  group <- group[,j]

  #Reordering group according to original x and y
  group <- group[order(i),]

  #Removing rownames
  rownames(df) <- NULL
  rownames(group) <- NULL

  return(list(df = df, group = group))
}
