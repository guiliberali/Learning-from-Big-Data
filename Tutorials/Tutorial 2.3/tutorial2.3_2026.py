# title: "Tutorial 2.3 - Upper Confidence Bound (Python)"
# author: "LfBD Team, 2026"
# date: "October 2026"

# In this tutorial, we will be covering the Upper Confidence Bound (UCB) method to solve multi-armed
# bandit problems. This file is the Python version of tutorial2.3_2026.R and follows the
# same structure: the cells marked with # %% correspond to the code chunks of the R version.

# %% setup

# Packages required for subsequent analysis.
# From a terminal: pip install numpy pandas matplotlib

import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# In this tutorial we use the helper functions to do some data preparation for us
from helper_functions import prepare_dataframes

os.chdir("C:/Users/josef/Desktop/Learning-from-Big-Data-main/Learning-from-Big-Data-main/tutorials/input")

# # Dataset

# For this tutorial, we will work with the Yahoo dataset, which was also used in tutorial 2.1.

# %% chunk 2

# reads in csv of Yahoo dataset (the first column holds the row numbers)
dfYahoo = pd.read_csv("yahoo_day1_10arms_tiny.csv", index_col=0)

# selects the two relevant columns from Yahoo dataset; arm shown to user and reward observed
dfYahoo = dfYahoo[["arm", "reward"]].copy()
dfYahoo["index"] = np.arange(1, len(dfYahoo) + 1)


# # UCB: notation

# As covered in the lecture, UCB methods decide on the arm to pick using an 'optimistic' estimate of
# the reward - e.g. which arm has the most potential to yield a high reward? This is done by picking
# the arm that has the highest combination of (1) the estimated reward, Q_t(a), and (2) a bonus that
# rewards the arm for being underexplored, denoted as U_t(a). The UCB algorithm picks the arm that
# maximizes the sum of these two factors:

#     a_t = argmax_a  Q_t(a) + c * U_t(a)

# In the general case of the UCB algorithm, U_t(a) = sqrt(log(t) / N_t(a)), where N_t(a) is the
# number of times arm a has been pulled so far.

# Task 1: select the arm according to the UCB algorithm based on the data below. Here, the code
# already provides a dataframe with per arm (1) the average reward, (2) the number of pulls thus
# far, and (3) a given value for t. Use this dataframe to determine which arm should be pulled if
# c = 0.1. The selected arm should be 8.

# %% chunk 3

# set the seed
np.random.seed(0)

c = 0.1

#####
# policy_ucb : picks an arm, based on the UCB algorithm for multi-armed bandits
#
# Arguments :
# df: DataFrame with two columns: arm, reward
# c : float, UCB penalty parameter
##
# Output :
# chosen_arm ; integer, index of the arm chosen
####
def policy_ucb(df, c):

    # get per item, the average reward and the number of items observed
    dfsummary = (
        df.groupby("arm", as_index=False)
        .agg(avg_reward=("reward", "mean"), n_pulls=("reward", "size"))
    )

    # the t in this case is simply the total of observations
    t = dfsummary["n_pulls"].sum()

    # TODO: Select an arm based on the UCB criterion (using equation (1))

    chosen_arm = None

    return chosen_arm


# get the chosen arm from the function
ucb_ichosen_item = policy_ucb(dfYahoo, c)
print("The chosen arm is: " + str(ucb_ichosen_item))


# # UCB: code

# We will be simulating the performance of the UCB algorithm on the Yahoo! data.

# %% simulator

###
# sim_ucb: simulates performance of a UCB policy
#
#   Arguments:
#
#     df: n x 3 DataFrame, with column names "arm", "reward", "index"
#
#     n_before_sim: integer, number of observations (randomly sampled)
#                   before starting the UCB algorithm
#
#     n_sim: integer, number of observations used to simulate the
#            performance of the UCB algorithm
#
#     c: float, UCB penalty parameter
#
#     interval: the number of steps after which our arm is updated.
#               For example, interval is 5 means that when an arm
#               is chosen by our approach, it is deployed for 5 steps.
#
#   Output: dict with the following
#     df_results_of_policy: n_sim x 2 DataFrame, with "arm", "reward".
#                           Random sampled rewards for chosen arms
#
#     df_sample_of_policy: DataFrame with sample used to evaluate policy.
###
def sim_ucb(df, n_before_sim, n_sim, c, interval=1):

    ## Part 1: create two dataframes, one with data before start of policy, and one with data after

    # define the number of observations of all data available
    n_obs = len(df)

    # Give user a warning: the size of the intended experiment is bigger than the data provided
    if n_sim > (n_obs - n_before_sim):
        raise ValueError("The indicated size of the experiment is bigger than the data provided - shrink the size")

    # Next we prepare our dataframes using a function from helper_functions.py
    prepped_data = prepare_dataframes(df, n_obs, n_before_sim, n_sim)

    # Access individual data frames
    df_results_policy = prepped_data["df_results_policy"]
    df_during_policy = prepped_data["df_during_policy"]
    df_results_at_t = prepped_data["df_results_at_t"]

    ## part 2: apply UCB algorithm, updating at interval
    for i in range(1, n_sim + 1):

        # update at interval
        if (i == 1) or (i % interval == 0):

            # TODO: pick an arm according to the UCB policy using the function from Task 1
            chosen_arm = None

        else:

            # TODO: if not updating, take current arm
            chosen_arm = None

        # select from the data for experiment the arm chosen
        df_during_policy_arm = df_during_policy[df_during_policy["arm"] == chosen_arm]

        # warn the user to increase dataset or downsize the size of experiment,
        # in the case that we have sampled all observations from an arm
        if len(df_during_policy_arm) == 0:
            print("You have run out of observations from a chosen arm")
            break

        # randomly sample from this arm and observe the reward
        sampled_arm = np.random.randint(0, len(df_during_policy_arm))
        reward = df_during_policy_arm["reward"].iloc[sampled_arm]

        # important: remove the sampled observation from the dataset to prevent repeated sampling
        index_result = df_during_policy_arm["index"].iloc[sampled_arm]
        df_during_policy = df_during_policy[df_during_policy["index"] != index_result]

        # add to dataframe to save the result
        df_results_policy.iloc[i - 1] = [chosen_arm, reward]

        # TODO: combine to dataframe with all results
        df_results_at_t = None

    # save results in a dict
    results = {
        "df_results_of_policy": df_results_policy,
        "df_sample_of_policy": df_during_policy,
    }

    return results


# Task 2: Run 10 simulations and calculate the cumulative reward per simulation (1-10).

# %% chunk 5

# number of observations used to simulate
n_sim = 2500

# The number of simulations
num_simulations = 10
c = 0.1

# set the seed
np.random.seed(0)

# Create a list where the results of the simulator are stored
results_ucb_01 = []

# Loop to get 10 simulations
for i in range(num_simulations):

    # TODO: run UCB simulation using the function built in the previous task
    df_Yahoo_UCB_01_temp = None

    # Append the results to our list
    results_ucb_01.append(df_Yahoo_UCB_01_temp)

df_Yahoo_UCB_01 = pd.concat(results_ucb_01, ignore_index=True)
df_Yahoo_UCB_01["simulation"] = np.repeat(np.arange(1, num_simulations + 1), n_sim)


# Task 3: repeat the steps of this tutorial, but now for c = 0.5.

# %% chunk 6

c = 0.5

# set the seed
np.random.seed(0)

# List where the results of the simulator are stored
results_ucb_05 = []

# Loop over the simulations
for i in range(num_simulations):

    # TODO: Run the UCB algorithm for c = 0.5
    df_Yahoo_UCB_05_temp = None

    # Append the results to our list to keep track of the results
    results_ucb_05.append(df_Yahoo_UCB_05_temp)

df_Yahoo_UCB_05 = pd.concat(results_ucb_05, ignore_index=True)
df_Yahoo_UCB_05["simulation"] = np.repeat(np.arange(1, num_simulations + 1), n_sim)


# Task 3: Make a plot that compares the UCB policy for c = 0.1 and c = 0.5. Compare the policies
# based on average cumulative reward. Can you conclude which one performs better, and if so why?

# Your answer to Task 3 here:

# %% chunk 7

# set max observations to create fair comparison across simulations
max_obs = 2500


def aggregate_history(df, num_simulations, max_obs, n_sim):

    df = df.copy()
    df["t"] = np.tile(np.arange(1, n_sim + 1), num_simulations)

    # per simulation, cumulative reward over time
    df["cumulative_reward"] = df.groupby("simulation")["reward"].cumsum()

    agg = (
        df.groupby("t", as_index=False)                       # group by timestep
        .agg(avg_cumulative_reward=("cumulative_reward", "mean"),
             sd_cumulative_reward=("cumulative_reward", "std"))
    )
    agg["se_cumulative_reward"] = agg["sd_cumulative_reward"]/np.sqrt(num_simulations)
    agg["cumulative_reward_lower_CI"] = (agg["avg_cumulative_reward"]
                                         - 1.96*agg["se_cumulative_reward"])
    agg["cumulative_reward_upper_CI"] = (agg["avg_cumulative_reward"]
                                         + 1.96*agg["se_cumulative_reward"])

    return agg[agg["t"] <= max_obs]


df_history_agg_01 = aggregate_history(df_Yahoo_UCB_01, num_simulations, max_obs, n_sim)
df_history_agg_05 = aggregate_history(df_Yahoo_UCB_05, num_simulations, max_obs, n_sim)

# combine the dataframes
df_history_agg_ucb = pd.concat([df_history_agg_01.assign(c="0.1"),
                                df_history_agg_05.assign(c="0.5")], ignore_index=True)

# TODO: make a plot to compare the UCB policy for c=0.1 and c=0.5
# 1: A plot that shows only the average cumulative rewards over time
# 2: The plot as defined in (1) together with the 95% confidence interval.

# Task 4: Suppose we compare the UCB policy for c = 0.1 to an epsilon-greedy policy where
# epsilon = 0.1, and find that the epsilon-greedy policy works better in this case. Why do you think
# this might happen?

# Answer:


# Task 5: Brain teaser

# Suppose the environment changes while your algorithm is in place. For example, imagine your
# algorithm's job is to serve news articles on a personalized news site. Here, the arms of the
# algorithm might be the topics of articles (politics, economy, sports, etc.)
# Your readers usually don't have much interest in sports-related articles, but it so happens
# that the world cup is being played. Suddenly, the readers show much greater interest in sports
# than is usual. However, the algorithm you have in place has no way of dealing with a change in
# preferences, which directly carry over into arm success rates (non-stationarity).
# Outline the two most conventional ways in which non-stationarity is dealt with, along with the
# modelling choices you have to make in each case.
