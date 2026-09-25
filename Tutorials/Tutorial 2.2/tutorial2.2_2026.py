# title: "Tutorial 2.2 - Thompson Sampling (Python)"
# author: "LfBD Team, 2026"
# date: "October 2026"

# In this tutorial, we will be covering the Thompson Sampling (TS) method to solve multi-armed
# bandit problems. This file is the Python version of tutorial2.2_2026.R and follows the same
# structure: the cells marked with # %% correspond to the code chunks of the R version.

# %% setup

# Packages required for subsequent analysis.
# From a terminal: pip install numpy pandas matplotlib

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# the input folder sits next to the tutorial folders
try:
    HERE = Path(__file__).resolve().parent
except NameError:  # when running cell by cell in an IDE, __file__ may not exist
    HERE = Path.cwd()
DATA_DIR = HERE.parent / "input"


# # Dataset

# For this tutorial, we will work with a dataset from ZOZO, the largest Japanese fashion e-commerce
# company. The company uses multi-armed bandit algorithms to recommend fashion items to users on
# its large-scale e-commerce platform.

# We will work with a version of the dataset that contains 10 fashion items. Each row contains a
# fashion item randomly shown to a user, and if the user clicked on said fashion item. The original
# dataset contains +170k observations. However, in this tutorial we will be using the version with
# 10k observations. The fashion items were randomly shown to users, which makes this dataset
# suitable for evaluating our TS algorithm.

# %% chunk 2

# this version contains 10 arms (ZOZO)
dfZozo_10 = pd.read_csv(DATA_DIR / "zozo_noContext_10items.csv", index_col=0)[["item_id", "click"]]

# sim_thompson() samples on 'index', so create it here (as is done for the 20-arm data in Task 5)
dfZozo_10["index"] = np.arange(1, len(dfZozo_10) + 1)


# Below the notation and code for Thompson Sampling is displayed.

# # Thompson Sampling: notation

# When working with the epsilon-greedy algorithm, we estimate the expected reward using the average
# reward up until that point. With Thompson Sampling, we take a different approach; instead of
# focusing solely on the expected reward, we try to find out the distribution of the reward. This
# is done in three steps:

#   1. The reward Q_t(a) for each arm follows a beta distribution. At the start, at t = 0, every
#      arm is assumed to follow the same beta distribution with parameters alpha_0 = 1 and
#      beta_0 = 1. This means we initially assume each arm has an equal probability of giving a
#      reward.
#   2. At each time t, the algorithm samples n_sample observations from each of the distributions.
#      The arm which has the highest average reward according to the sample is the chosen arm.
#   3. We then observe the reward r_t, and update the parameters of the distribution accordingly.
#        alpha_t = alpha_(t-1) + r_t
#        beta_t  = beta_(t-1) + 1 - r_t

# Task 1: select from the dataframe below an arm according to the Thompson Sampling algorithm.
# Per arm, the alpha and beta parameters have been given. Use n_sample = 100.

# %% chunk 3

# set the seed
np.random.seed(0)

# arm index
arm = np.arange(1, 11)

# alpha per arm
alpha = np.array([10, 2, 9, 3, 1, 8, 7, 4, 6, 5])

# beta per arm
beta = np.array([1, 8, 3, 2, 4, 7, 6, 5, 9, 10])

# dataframe with alpha, beta per arm
df_alpha_beta = pd.DataFrame({"arm": arm, "alpha": alpha, "beta": beta})

# number of samples to draw
n_sample = 1

#####
# policy_thompson: picks an arm, based on the Thompson algorithm for multi-armed bandits
#
# Arguments:
# df_alpha_beta: DataFrame with three columns: arm, alpha, and beta
# n_sample: integer, the total number of samples to draw
#
# Output:
# chosen_arm; integer, index of the arm chosen
####
def policy_thompson(df_alpha_beta, n_sample):

    # create an array with sampled values - one draw per arm (n_sample = 1)
    df_sampled = np.random.beta(df_alpha_beta["alpha"].values,
                                df_alpha_beta["beta"].values)

    # TODO: select an arm based on step 2 of TS (the arm with the highest average reward)

    chosen_arm = None

    # return the chosen arm
    return chosen_arm


# get the chosen arm from the function
chosen_arm_practice = policy_thompson(df_alpha_beta, n_sample)
print("The chosen arm is: " + str(chosen_arm_practice))


# # Thompson Sampling: code

# We will be simulating the performance of the Thompson Sampling algorithm on the Zozo data. For
# each simulation, one needs to define a (1) bandit, which draws the rewards per arm, (2) a
# policy/algorithm for pulling arms of the bandit, and (3) a simulator which simulates the
# performance of the policy/algorithm given the bandit. The bandit is the same as in Tutorial 2.1.
# The Thompson policy is given in Task 1, see above.

# Task 2: build a simulator function. Initialize the alpha and beta parameters, and update these
# based on the sampled rewards.

# %% simulator

###
# sim_thompson: simulates performance of a Thompson policy
#
#   Arguments:
#
#     df: n x 3 DataFrame, with column names "item_id", "click", "index"
#
#     n_sim: integer, number of observations used to simulate the
#            performance of the Thompson algorithm
#
#     n_sample: integer, the total number of samples to draw
#
#     interval: the number of steps after which our arm is updated.
#               For example, interval is 5 means that when an arm
#               is chosen by our approach, it is deployed for 5 steps.
#
#   Output: dict with the following
#     df_results_of_policy: n_sim x 2 DataFrame, with "arm", "reward".
#                           Random sampled rewards for chosen arms
#
#     df_alpha_beta: DataFrame with alphas and betas for each arm.
###
def sim_thompson(df, n_sim, n_sample, interval=1):

    # define the number of observations of all data available
    n_obs = len(df)

    # define the number of arms
    n_arms = df.iloc[:, 0].max()

    # Give user a warning: the size of the intended experiment is bigger than the data provided
    if n_sim > n_obs:
        raise ValueError("The indicated size of the experiment is bigger than the data provided - shrink the size")

    # arm index
    arm = np.arange(1, n_arms + 1)

    # alpha per arm
    alpha = np.ones(n_arms)

    # beta per arm
    beta = np.ones(n_arms)

    # dataframe with alpha, beta per arm
    df_alpha_beta_at_t = pd.DataFrame({"arm": arm, "alpha": alpha, "beta": beta})

    # create dataframe with data that we can sample from during policy
    df_during_policy = df

    # Empty dataframe where the results of the policy are stored
    df_results_policy = pd.DataFrame(np.nan, index=range(n_sim), columns=["arm", "reward"])

    ## part 2: apply Thompson algorithm, updating at interval
    for i in range(1, n_sim + 1):

        # update at interval
        if (i == 1) or (i % interval == 0):

            # TODO: choose arm at interval with our Thompson policy function
            chosen_arm = None

        else:

            # TODO: In the case that we do not update, take the current arm
            chosen_arm = None

        # select from the data for experiment the arm chosen
        df_during_policy_arm = df_during_policy[df_during_policy["item_id"] == chosen_arm]

        # warn the user to increase dataset or downsize the size of experiment,
        # in the case that we have sampled all observations from an arm
        if len(df_during_policy_arm) == 0:
            print("You have run out of observations from a chosen arm")
            break

        # randomly sample from this arm and observe the reward
        sampled_arm = np.random.randint(0, len(df_during_policy_arm))
        reward = df_during_policy_arm["click"].iloc[sampled_arm]

        # important: remove the reward from the dataset to prevent repeated sampling
        index_result = df_during_policy_arm["index"].iloc[sampled_arm]
        df_during_policy = df_during_policy[df_during_policy["index"] != index_result]

        # store the results of the chosen arm (arm, reward)
        df_results_policy.iloc[i - 1] = [chosen_arm, reward]

        # TODO: update alpha and beta parameters
        # df_alpha_beta_at_t.loc[..., "alpha"] =
        # df_alpha_beta_at_t.loc[..., "beta"] =

    # save results in a dict
    results = {
        "df_results_of_policy": df_results_policy,
        "df_alpha_beta": df_alpha_beta_at_t,
    }

    return results


# Task 3: Run 10 simulations and calculate the cumulative reward per simulation (1-10).

# %% chunk 5

# number of observations used to simulate
n_sim = 2500

# The number of simulations
num_simulations = 10

# set the seed for reproducability
np.random.seed(0)

# Again, we create a list where the results of the simulator are stored
results_per_simulation = []

# Loop over each of the 10 simulations
for i in range(num_simulations):

    # TODO: run Thompson simulation using the function built in Task 2 using interval = 1
    df_TS_zozo_10_temp = None

    # Append the results of the simulation to our results
    results_per_simulation.append(df_TS_zozo_10_temp)

df_TS_zozo_10 = pd.concat(results_per_simulation, ignore_index=True)
df_TS_zozo_10["simulation"] = np.repeat(np.arange(1, num_simulations + 1), n_sim)


# TODO: Calculate the cumulative reward per simulation
df_TS_zozo_10_total_reward = None

print(df_TS_zozo_10_total_reward)


# In the dataframe df_TS_zozo we have gathered the results of the TS policy. This dataframe
# contains the following columns:
#   - arm: the choice made by the algorithm
#   - reward: the reward observed by the algorithm
#   - simulation: the simulation the row belongs to

# Task 4: using this dataframe, make a plot of the average cumulative rewards for all simulations,
# together with the 95% confidence interval.

# %% chunk 6

# Create an additional column to keep track of the timestamps
df_TS_zozo_10["t"] = np.tile(np.arange(1, n_sim + 1), num_simulations)

# Max of observations.
# You may want to adjust this depending on the number of observations per simulation and the size
# of the dataset used
max_obs = 9000

# Dataframe transformation
df_TS_zozo_10["cumulative_reward"] = (
    df_TS_zozo_10.groupby("simulation")["reward"].cumsum()  # per sim, cumulative reward over time
)
df_history_agg = (
    df_TS_zozo_10
    .groupby("t", as_index=False)                           # group by timestep
    .agg(avg_cumulative_reward=("cumulative_reward", "mean"),
         sd_cumulative_reward=("cumulative_reward", "std"))
)
df_history_agg["se_cumulative_reward"] = (
    df_history_agg["sd_cumulative_reward"] / np.sqrt(num_simulations)
)
df_history_agg["cumulative_reward_lower_CI"] = (
    df_history_agg["avg_cumulative_reward"] - 1.96 * df_history_agg["se_cumulative_reward"]
)
df_history_agg["cumulative_reward_upper_CI"] = (
    df_history_agg["avg_cumulative_reward"] + 1.96 * df_history_agg["se_cumulative_reward"]
)
df_history_agg = df_history_agg[df_history_agg["t"] <= max_obs]


# TODO: Make the following two plots:
# 1: A plot that shows only the average cumulative rewards over time using the df_history_agg
#    dataframe
# 2: The plot as defined in (1) together with the 95% confidence interval.


# As stated at the start, we only have 10 arms for the ZOZO data. However, there are also versions
# of the data with 20 arms. This is loaded for you below.

# Task 5: repeat the simulation steps in the tutorial above, but now for the data with 20 arms.

# %% load 20 arms

# this version contains 20 arms
dfZozo_20 = pd.read_csv(DATA_DIR / "zozo_noContext_20items_tiny.csv", index_col=0)[["item_id", "click"]]
dfZozo_20["index"] = np.arange(1, len(dfZozo_20) + 1)


# %% simulate 20 arms

n_sim = 2500
num_simulations = 10

# set the seed
np.random.seed(0)

# Again, we create a list where the results of the simulator are stored
results_per_simulation_20 = []

# Loop over each of the 10 simulations
for i in range(num_simulations):

    # TODO: run Thompson simulation using the function built in Task 2 for the dataset with 20 arms
    df_TS_zozo_20_temp = None

    # Append the results of the simulation to our results
    results_per_simulation_20.append(df_TS_zozo_20_temp)

df_TS_zozo_20 = pd.concat(results_per_simulation_20, ignore_index=True)
df_TS_zozo_20["simulation"] = np.repeat(np.arange(1, num_simulations + 1), n_sim)


# Task 6: Make a plot that compares the average cumulative reward (with 95% confidence interval)
# for 10 and 20 arms. For which version does Thompson Sampling perform better?

# %% compare 10 and 20 arms

# set max observations to create fair comparison across simulations
max_obs = 2000
df_TS_zozo_20["t"] = np.tile(np.arange(1, n_sim + 1), num_simulations)

df_TS_zozo_20["cumulative_reward"] = (
    df_TS_zozo_20.groupby("simulation")["reward"].cumsum()  # calculate cumulative reward
)
df_history_agg_20 = (
    df_TS_zozo_20
    .groupby("t", as_index=False)                           # group by timestep t
    .agg(avg_cumulative_reward=("cumulative_reward", "mean"),
         sd_cumulative_reward=("cumulative_reward", "std"))
)
df_history_agg_20["se_cumulative_reward"] = (
    df_history_agg_20["sd_cumulative_reward"] / np.sqrt(num_simulations)
)
df_history_agg_20["cumulative_reward_lower_CI"] = (
    df_history_agg_20["avg_cumulative_reward"] - 1.96 * df_history_agg_20["se_cumulative_reward"]
)
df_history_agg_20["cumulative_reward_upper_CI"] = (
    df_history_agg_20["avg_cumulative_reward"] + 1.96 * df_history_agg_20["se_cumulative_reward"]
)
df_history_agg_20 = df_history_agg_20[df_history_agg_20["t"] <= max_obs]

# Combine the dataframes
df_history_agg_both = pd.concat([
    df_history_agg.assign(no_arms="10"),
    df_history_agg_20.assign(no_arms="20"),
], ignore_index=True)

# TODO: Make the following two plots:
# 1: A plot that compares the average cumulative rewards over time for 10 and 20 arms using the
#    df_history_agg_both dataframe
# 2: The plot as defined in (1) together with the 95% confidence interval.


# Task 7: Brain teaser

# Will be provided by the professor during lecture.
