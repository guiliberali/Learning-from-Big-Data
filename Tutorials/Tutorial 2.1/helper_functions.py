# Helper functions for Tutorial 2.1 (Python version of helper_functions.R)

import numpy as np
import pandas as pd


def prepare_dataframes(df, n_obs, n_before_sim, n_sim):
    """Split the data into a pre-policy sample and the data used during the policy.

    Returns a dict with:
      df_results_policy: empty n_sim x 2 frame where the policy results are stored
      df_during_policy : the observations the policy can still sample from
      df_results_at_t  : the observations gathered before the policy starts
    """

    # find n_before_sim random observations to be used before start policy
    index_before_sim = np.random.choice(n_obs, size=n_before_sim, replace=False)

    # using indexing, create dataframe with data before start policy
    df_before_policy = df.iloc[index_before_sim]

    # save dataframe with all the results at t - to begin with those before the policy
    df_results_at_t = df_before_policy[["arm", "reward"]].copy().reset_index(drop=True)

    # create dataframe with data that we can sample from during policy
    df_during_policy = df.drop(df.index[index_before_sim])

    # dataframe where the results of storing the policy are stored
    df_results_policy = pd.DataFrame(np.nan, index=range(n_sim), columns=["arm", "reward"])

    return {
        "df_results_policy": df_results_policy,
        "df_during_policy": df_during_policy,
        "df_results_at_t": df_results_at_t,
    }
