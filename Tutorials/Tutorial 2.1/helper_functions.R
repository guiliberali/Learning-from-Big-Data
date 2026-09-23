prepare_dataframes <- function(df, n_obs, n_before_sim, n_sim){
  
  # find n_before_sim random observations to be used before start policy
  index_before_sim <- sample(1:n_obs,n_before_sim)
  
  # using indexing, create dataframe with data before start policy
  df_before_policy <- df[index_before_sim,]
  
  # save dataframe with all the results at t - to begin with those before the policy
  df_results_at_t <- df_before_policy %>% 
    select(arm, reward)
  
  # create dataframe with data that we can sample from during policy
  df_during_policy <- df[-index_before_sim,]
  
  # dataframe where the results of storing the policy are stored
  df_results_policy <- data.frame(matrix(NA, nrow = n_sim, ncol = 2))
  colnames(df_results_policy) <- c('arm', 'reward')
  
  return(list(
    df_results_policy = df_results_policy,
    df_during_policy = df_during_policy,
    df_results_at_t = df_results_at_t
  ))
}