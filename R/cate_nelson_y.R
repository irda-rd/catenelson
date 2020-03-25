#' @rdname cate_nelson
#' @export
cate_nelson_y <- function(y, group_x, min_group = 1, trend = "positive"){
  #Order dataset by x
  i <- order(y)
  y <- y[i]
  group_x <- group_x[i,,drop = FALSE]

  #Identify n_group from group_x
  n_group <- group_nb(group_x)

  #Generate possible partitions and associated selection matrices
  n <- length(y)
  division  <- group_division(y, n_group, min_group = min_group)
  group_y <- group_matrix(n, division)

  #Compute the number of elements in each quadrat
  Q <- quadrat_count(group_x, group_y)

  #Summarise and export
  n_pred <- quadrat_count_pred(Q, trend = trend)
  n <- length(y)
  n_err <- n - quadrat_count_pred(Q, trend = trend)
  p_pred = n_pred/n
  p_err = n_err/n
  df_Q_summary <- data.frame(n_pred = n_pred, n_err = n_err, n = n, p_pred = p_pred, p_err = p_err)
  df_Q <- Q
  colnames(df_Q) <- paste0("n_", colnames(df_Q))

  #Compute mid values associated to y divisions
  df_crit_y <- as.data.frame(t(group_crit(y, division)), stringsAsFactors = FALSE)
  names(df_crit_y) <- paste0("crit_y_", seq_len(nrow(division)))

  #Compute supplementary statistics on quadrats
  df_stat <- quadrat_stat(Q)

  #Join results as df
  df <- cbind(df_crit_y, df_Q, df_Q_summary, df_stat)

  #Ordering df and group in decreasing order of n_pred then Cramer's V
  j <- order(df$n_pred, df$cramer_V, decreasing = TRUE)
  df <- df[j,]
  group_y <- group_y[,j]
  Q <- Q[j,]

  #Reordering group according to original y
  group <- group_y[order(i),]

  #Removing rownames
  rownames(df) <- NULL
  rownames(Q) <- NULL
  rownames(group) <- NULL

  return(list(df = df, Q = Q, group = group))
}
