# title: "Tutorial 2.1 - Epsilon-Greedy and Calculating Regret (Python, Solutions)"
# author: "LfBD Team, 2026"
# date: "September 2026"

# # Introduction

# In this problem set, you will be implementing two concepts.

#   - The epsilon-Greedy Algorithm: In short, this algorithm picks in epsilon % of cases a random
#     arm (exploration). In 1-epsilon % of cases, pick the arm with at that point the highest
#     average reward (exploitation).
#   - Regret calculation: in this case, regret means the reward the algorithm did not obtain
#     because it did not play the optimal arm. This means the difference between the rewards
#     obtained by pulling the chosen arms (in this case, the arms selected by the epsilon-Greedy
#     algorithm) and the rewards that would have been obtained had we pulled the optimal arms.

# In addition to the concepts mentioned above, we will use more dataframe operations and apply data
# subsetting. This file is the Python version of tutorial2.1_2026_solutions.R and follows the same
# structure: the cells marked with # %% correspond to the code chunks of the R version.

# # Loading libraries

# Before starting the problem set, make sure you have all the libraries installed that are needed.
# From a terminal: pip install numpy pandas matplotlib

# %% setup

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# Additionally we load some custom functions to help us later on
from helper_functions import prepare_dataframes

# the input folder sits next to the tutorial folders
try:
    HERE = Path(__file__).resolve().parent
except NameError:  # when running cell by cell in an IDE, __file__ may not exist
    HERE = Path.cwd()
DATA_DIR = HERE.parent / "input"


# # Dataset

# We will explore the concepts from this problem set with a dataset from Yahoo!. This dataset
# contains information on 10 articles, detailing when each article was shown to a user and whether
# the user clicked on it. The data is from a single day and contains +40k observations. If your
# laptop cannot handle the size of this dataset, we have provided a smaller version with fewer
# observations for you to use. Please note that results may vary depending on which dataset you
# use. The dataset contains three columns:

#   - arm: the index of the article shown to the user (1-10)
#   - reward: 0 if a user did not click on the article, 1 if a user did click on the article
#   - index: unique id of the arm-reward combination.

# Each observation is an arm and the respective reward for showing that arm to a user. The articles
# were randomly shown to users, which makes this dataset suitable for evaluating our epsilon-Greedy
# Algorithm.

# %% load data

# reads in csv of Yahoo dataset (the first column holds the row numbers)
dfYahoo = pd.read_csv(DATA_DIR / "yahoo_day1_10arms_tiny.csv", index_col=0)

# selects the two relevant columns from Yahoo dataset; arm shown to user and reward observed
dfYahoo_for_sim = dfYahoo[["arm", "reward"]].copy()

# Create an index column
dfYahoo_for_sim["index"] = np.arange(1, len(dfYahoo_for_sim) + 1)


# # epsilon-Greedy Algorithm: notation

# First, a brief recap of key notation from the lecture that will be useful for this tutorial.

# With a denoting an arm, a in A indicates that the arm a is one of the options available to our
# algorithm from the set of all available arms, A. We define a_t as the chosen arm at time t. Our
# goal is to select arms in a way that gives us the highest possible reward, Q_t(a). In the case of
# an epsilon-Greedy Algorithm, we pursue the greedy option in (1-epsilon)% of cases, and in
# epsilon% of cases we select a random arm. Thus, epsilon determines the degree of exploitation
# (greedy) vs. exploration (random choice). More formally, our chosen arm becomes:

#     a_t = argmax_a Q_t(a)   with probability 1-epsilon   (exploitation)
#     a_t = a random arm      with probability epsilon     (exploration)

# # epsilon-Greedy Algorithm: code

# We will code up our own version of the epsilon-Greedy algorithm in two steps. First, you will
# code a function that, based on a set of arms and rewards, selects an arm according to the
# epsilon-Greedy algorithm. Second, this tutorial will provide a function for simulating the
# epsilon-Greedy algorithm on the Yahoo dataset.

# Task 1: finish the function specified below

# Given a set of arms and rewards, we want to write a function, to be called policy_greedy, that
# selects an arm. It should do so in line with equation (1).

# IMPORTANT: The outline of the function is specified below - you only have to fill in the part
# that states TODO.

# Your function should return an integer which indexes which arm (1-10) is the chosen arm. In order
# to check if your code works, set epsilon = 0 - it then should return 3. Then, set epsilon = 0.5.
# Now, in 50% of cases it should return 3, and in 50% of cases another random number.

# %% function for greedy pick

# Grab the first 100 observations, create dataframe to practice for policy_greedy() function
df_practice = dfYahoo_for_sim.iloc[:100]

# set value of epsilon parameter
eps = 0

#####
# policy_greedy : This function picks an arm, based on the greedy algorithm for multi-armed bandits
#
# The policy_greedy function takes the following three arguments as input :
#  df: this is a DataFrame with three columns: arm, reward, and index
#  eps : a float, which is the percentage of times a random arm needs to be picked
#  n_arms: an integer, which is the total number of arms available. Default value is set to 10.
#
# The function outputs/returns :
# chosen_arm ; integer, index of the arm chosen
####
def policy_greedy(df, eps, n_arms=10):

    # Draws a random float between 0 and 1 from a uniform distribution
    random_uniform_variable = np.random.uniform(0, 1)

    # in epsilon % of cases, pick a random arm (exploration)
    if random_uniform_variable < eps:

        # TODO: sample uniform random arm between 1 and n_arms
        chosen_arm = np.random.randint(1, n_arms + 1)

    # in (1-epsilon)% of cases, pick the arm that has the highest average reward
    else:

        # TODO: finish the code below to create the dataframe df_reward_overview
        # this dataframe shows for each arm:
        # - reward_size: total reward
        # - sample_size: total observations
        # - succes_rate: total reward/total observations

        df_reward_overview = (
            df.dropna()
            .groupby("arm", as_index=False)
            .agg(reward_size=("reward", "sum"), sample_size=("reward", "size"))
        )
        df_reward_overview["succes_rate"] = (
            df_reward_overview["reward_size"] / df_reward_overview["sample_size"]
        )

        # TODO: pick the arm with the highest succes_rate from df_reward_overview
        # note: we read off the arm label, which is safer than the row position used by which.max()
        # in the R version - the two agree as long as every arm appears in df
        chosen_arm = df_reward_overview.loc[df_reward_overview["succes_rate"].idxmax(), "arm"]

    # return the chosen arm
    return chosen_arm


# Now that we have this function, we can use it to simulate an epsilon-Greedy algorithm for the
# Yahoo dataset. Before the simulation starts, there is an initial period in which we gather some
# information about the arms. In this period, users are randomly exposed to articles. Based on this
# initial information, we then start our epsilon-Greedy algorithm to pick the articles. For
# n_before_sim steps we gather information, and for n_sim steps we simulate our algorithm.

# Task 2: finish the function specified below

# %% function for sim

###
# sim_greedy: simulates performance of an epsilon-greedy policy
#
#   Arguments:
#
#     df: DataFrame with column names "arm", "reward", and "index"
#
#     n_before_sim: integer, number of observations (randomly sampled)
#                   before starting the epsilon-greedy algorithm
#
#     n_sim: integer, number of observations used to simulate the
#            performance of the epsilon-greedy algorithm
#
#     epsilon: float between 0 and 1. In  epsilon%  of cases,
#              randomly explore other arms. In 1-epsilon %, pick arm
#              that has highest average reward up until that point.
#
#     interval: the number of steps after which our arm is updated.
#               For example, interval is 5 means that when an arm
#               is chosen by our approach, it is deployed for 5 steps.
#               The default value is set to 1.
#
#   Output: dict with the following
#     df_results_of_policy: n_sim x 2 DataFrame, with "arm", "reward".
#                           Random sampled rewards for chosen arms
#
#     df_sample_of_policy: DataFrame with sample used to evaluate policy.
###
def sim_greedy(df, n_before_sim, n_sim, epsilon, interval=1):

    ## Part 1: create two dataframes, one with data before start of policy, and one with data after

    # define the number of observations of all data available
    n_obs = len(df)

    # Give user a warning: the size of the intended experiment is bigger than the data provided
    if n_sim > (n_obs - n_before_sim):
        raise ValueError("The indicated size of the experiment is bigger than the data provided - shrink the size")

    # Next we prepare our dataframes using functions from helper_functions.py
    prepped_data = prepare_dataframes(df, n_obs, n_before_sim, n_sim)

    # Access individual data frames
    df_results_policy = prepped_data["df_results_policy"]
    df_during_policy = prepped_data["df_during_policy"]
    df_results_at_t = prepped_data["df_results_at_t"]

    ## part 2: apply epsilon-greedy algorithm, updating at interval
    for i in range(1, n_sim + 1):

        # update at interval
        if (i == 1) or (i % interval == 0):

            # TODO: select the arm with your policy_greedy function and define the current arm
            chosen_arm = policy_greedy(df_results_at_t, epsilon)
            current_arm = chosen_arm

        else:

            # TODO: In the case that we do not update, take the current arm
            chosen_arm = current_arm

        # select from the data for experiment the arm chosen
        df_during_policy_arm = df_during_policy[df_during_policy["arm"] == chosen_arm]

        # warn the user to increase dataset or downsize the size of experiment,
        # in the case that we have sampled all observations from an arm
        if len(df_during_policy_arm) == 0:
            raise ValueError("You have run out of observations from a chosen arm")

        # randomly sample from this arm and observe the reward
        sampled_arm = np.random.randint(0, len(df_during_policy_arm))
        reward = df_during_policy_arm["reward"].iloc[sampled_arm]

        # important: remove the sampled observation from the dataset to prevent repeated sampling
        index_result = df_during_policy_arm["index"].iloc[sampled_arm]
        df_during_policy = df_during_policy[df_during_policy["index"] != index_result]

        # store the results of the chosen arm (arm, reward)
        df_results_policy.iloc[i - 1] = [chosen_arm, reward]

        # TODO: combine to dataframe with all results
        df_results_at_t = pd.concat(
            [df_results_at_t, pd.DataFrame([{"arm": chosen_arm, "reward": reward}])],
            ignore_index=True,
        )

    # save results in a dict
    results = {
        "df_results_of_policy": df_results_policy,
        "df_sample_of_policy": df_during_policy,
    }

    return results


# Task 3: simulate the epsilon greedy algorithm

# Using the sim_greedy function, run the epsilon greedy algorithm for the following parameters:
# n_before_sim = 20, n_sim = 200, epsilon = 0.1 and epsilon = 0.25. Calculate the total reward
# based on the returned df_results_of_policy.

# %% simulate greedy algorithm

# set parameters: observations before start simulation, then how many observations for simulation
n_before_sim = 20
n_sim = 200

# set the seed for reproducability
np.random.seed(0)

# Use the sim_greedy function created in task 2 to simulate the results for epsilon = 0.1
result_sim_eps_01 = sim_greedy(dfYahoo_for_sim,
                               n_before_sim=n_before_sim,
                               n_sim=n_sim,
                               epsilon=0.1,
                               interval=1)

# TODO: use the sim_greedy function created in task 2 to simulate the results for epsilon = 0.25
# assign the results to a variable called result_sim_eps_025
result_sim_eps_025 = sim_greedy(dfYahoo_for_sim,
                                n_before_sim=n_before_sim,
                                n_sim=n_sim,
                                epsilon=0.25,
                                interval=1)


# Task 4: plot performance for t

# Compare the performance of the algorithm for epsilon = 0.1 and epsilon = 0.25 using a plot. The
# x-axis should show the steps t, and the y-axis the cumulative reward at point t. Which one of
# these performs better? Give a general explanation why we would expect this version of the
# algorithm to perform better?

# %% create plot

# create a dataframe with the results per epsilon.
# calculate the cumulative sum over time
df_result_sims = pd.DataFrame({
    "t": np.arange(1, n_sim + 1),
    "eps_01": result_sim_eps_01["df_results_of_policy"]["reward"].cumsum().values,
    "eps_025": result_sim_eps_025["df_results_of_policy"]["reward"].cumsum().values,
})

# TODO: Plot the reward in a line graph to compare performance for epsilon = 0.1 and epsilon = 0.25
fig, ax = plt.subplots(figsize=(7, 4.5))
ax.plot(df_result_sims["t"], df_result_sims["eps_01"], color="darkblue", label="0.1")
ax.plot(df_result_sims["t"], df_result_sims["eps_025"], color="red", label="0.25")
ax.set_xlabel("Time")
ax.set_ylabel("TotalReward")
ax.legend(title="epsilon")
fig.tight_layout()
plt.show()


# Extra: if you have time left, consider implementing a grid-search, which is a method used to
# evaluate every possible combination of selected parameter values (e.g. for n_before_sim,
# interval, and epsilon) to determine the combination leading to the highest reward.


# # Calculating regret

# Regret is the opportunity loss given what we could have received, and what we did receive. The
# optimal value, denoted by V*, is the highest possible average reward we can achieve by selecting
# the best arm from the set of all arms. The total regret at time t is then:

#     L_t = E[ sum over tau of (V* - Q(a_tau)) ]

# Intuitively, it measures the difference between selecting the arm with the best average reward
# and the arms chosen by our algorithm.

# Task 5: calculate regret for epsilon = 0.1

# Based on equation (2), calculate the regret for the greedy algorithm when epsilon = 0.1.

# %% calculating regret

# set the seed
np.random.seed(0)

# total reward for our algorithm
total_reward_eps_01 = df_result_sims["eps_01"].iloc[-1]

# obtain the average reward per arm
df_result_per_arm_sample = (
    result_sim_eps_01["df_sample_of_policy"]
    .groupby("arm", as_index=False)
    .agg(avg_reward=("reward", "mean"))
)

# TODO: calculate regret for the greedy algorithm when epsilon = 0.1 by using the following steps:
#   1. Select the average reward of the best arm
#   2. Determine the total reward of the best arm
#   3. Calculate the regret

# average reward of best arm
avg_reward_best_arm = df_result_per_arm_sample["avg_reward"].max()

# Total reward of the best arm
total_reward_best_arm = round(n_sim * avg_reward_best_arm)

# regret
regret = total_reward_best_arm - total_reward_eps_01


# Task 6: Brain teaser 1

# Suppose that you are working for a company implementing this algorithm, but you come across a
# problem. The company has millions of customers, and your algorithm has to handle huge amounts of
# data. As a consequence, choosing an arm with your algorithm can take very long, and importantly,
# cost you money when outsourcing the computation online. So, you have come up with a solution. You
# would like to aggregate the data into batches, where you only recompute the optimal arm every k
# observations. This way, you can avoid some of the computational cost. However, you are worried
# this will hurt the performance of your algorithm. Thus, you decide to investigate how the
# algorithm performs over a range of k's.

# To calculate the performance of the algorithm for each k, assume the following: A succesfull arm
# pull gives a reward of 1. Plot total reward over a chosen range of k.

# What you should do: set out some range of k's that you would like to test over. Run the
# sim_greedy() function over these k. For each k, run the algorithm 100 times so you can get
# confidence intervals for the reward of each k policy. Finally, plot both total reward over all
# chosen k, and reason which k you think is best.

# Answer: k is the interval argument of sim_greedy(). A small k recomputes the arm often and so
# carries the computational cost the company wants to avoid; a large k keeps a possibly wrong arm
# deployed for k observations, which could cost reward. Since reward is what we measure here, run
# the policy over a grid of k, repeat each k many times because a single run is noisy, and compare
# the mean total reward with its 95% confidence interval.

# %% brain teaser 1

# grid of update intervals, and the number of runs to average over
k_values = [1, 2, 5, 10, 25, 50, 100, 200]
num_reps = 100  # reward is noisy, so average over many runs (takes a couple of minutes)

np.random.seed(0)

rows = []
for k in k_values:

    # total reward collected by the policy, for num_reps runs at this k
    runs = np.array([
        sim_greedy(dfYahoo_for_sim,
                   n_before_sim=n_before_sim,
                   n_sim=n_sim,
                   epsilon=0.1,
                   interval=k)["df_results_of_policy"]["reward"].sum()
        for _ in range(num_reps)
    ])

    rows.append({
        "k": k,
        "total_reward": runs.mean(),
        "se_reward": runs.std(ddof=1) / np.sqrt(num_reps),
    })

df_k = pd.DataFrame(rows)

print(df_k)

# the k with the highest average reward
best_k = df_k.loc[df_k["total_reward"].idxmax(), "k"]
print(best_k)

# Plot: total reward over k, with 95% confidence intervals
fig, ax = plt.subplots(figsize=(7, 4.5))
ax.errorbar(df_k["k"], df_k["total_reward"], yerr=1.96 * df_k["se_reward"],
            fmt="-o", color="darkblue", ecolor="grey", capsize=3)
ax.set_xscale("log")
ax.set_xticks(k_values)
ax.set_xticklabels(k_values)
ax.set_xlabel("k (observations between updates)")
ax.set_ylabel("Total reward")
ax.set_title("Total reward over k (mean of " + str(num_reps) + " runs)")
fig.tight_layout()
plt.show()

# Reading the plot answers the question. Total reward turns out to be essentially flat in k on this
# data, k does not influence the success of the algorithm much: the confidence intervals of every k
# overlap, so we cannot say that updating every observation beats updating every 10th or every
# 100th.

# A reasonable k is thus the largest one at which the algorithm still works well - about k = 10
# here. Updating arm choice more often does not buy more accuracy, while every extra recalculation
# is computation the company pays for. Updating less often increases risk. Note that changing
# n_sim, the number of arms, or how far apart the arms' click rates lie can change the conclusion
# here.
