# title: "Tutorial 2.4 - Contextual bandits (Python)"
# author: "LfBD Team, 2026"
# date: "January 2026"

# All previous algorithms that we considered were only based on the observed past rewards, and the
# weighing of uncertainty around those. But obviously, there is a myriad of other forms of
# information that we can use to assess which arm will yield the most reward. A contextual
# multi-armed bandit uses this information (referred to as context) to decide which arm to pick at
# each timestep. This tutorial is about a contextual version of the UCB algorithm called linear
# UCB, and it is illustrated with an example of cryptocurrency markets.

# This file is the Python version of tutorial2.4_2026.R and follows the same structure:
# the cells marked with # %% correspond to the code chunks of the R version.

# # Theory: contextual UCB

# Recall the UCB method for selecting an arm a_t:

#     a_t = argmax_a  Q_t(a) + c * U_t(a),   with U_t(a) = sqrt(log(t) / N_t(a))

# With linear UCB we model a linear relationship between contextual features and the reward per
# arm. Let x_{a,t} be the row of features for arm a at time t. Linear UCB estimates:

#     a_t      = argmax_a  x_{a,t}' theta_a + c * sqrt(x_{a,t}' A_a^-1 x_{a,t})
#     theta_a  = A_a^-1 b_a
#     A_a      = A_a + x_{a,t} x_{a,t}'
#     b_a      = b_a + x_{a,t} r_{a,t}

#   - the estimated reward for an arm is a regression of its past rewards on its past context
#   - the uncertainty is the standard deviation of that estimate at the current context

# # Example of usefulness of context: trading cryptocurrencies

# The file 'df_cryptocurrencies.csv' contains, per coin and per day:
#   - Symbol: which cryptocurrency the row refers to
#   - returns: the return of holding that coin on that day
#   - Date: the day the returns are recorded
#   - obv: On-Balance Volume, a measure of changes in purchase volume
#   - roll_returns_week: average returns over the last 5 trading days
#   - prev_returns: return of the coin on the previous trading day

# Every coin is recorded on every date, so the return of each arm is observable at each step and
# no observation has to be discarded.

# %% chunk 1

import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

os.chdir("C:/Users/josef/Desktop/Learning-from-Big-Data-main/Learning-from-Big-Data-main/tutorials/input")

# get the dataframe with coins
df_coins = pd.read_csv("df_cryptocurrencies.csv")
df_coins = df_coins.drop(columns=["X"])
df_coins["obv"] = df_coins["obv"]/1000000

# number the days 1, 2, 3, ... : the file is ordered by date, four coins per date
df_coins["Date"] = np.repeat(np.arange(1, len(df_coins)//4 + 1), 4)


# Task 1: implement the Linear UCB policy. Complete the function below according to the formulae
# above.

# %% chunk 2

#####
# policy_linearucb: picks an arm, based on the linear UCB algorithm
#
# Arguments:
# context: dict with the data ('X'), the number of features ('d') and the arms
# vars: list of column names, the first of which identifies the arm
# theta: dict with the matrices A and the vectors b, one per arm
# c: float, linear UCB penalty parameter
# i: integer, the current time step
#
# Output:
# chosen_arm; integer, index of the arm chosen
####
def policy_linearucb(context, vars, theta, c, i, rng):

    expected_rewards = np.zeros(context["n_arms"])

    Xt = context["X"][context["X"]["Date"] == i]

    for k, arm in enumerate(context["arms"]):

        Xa = Xt.loc[Xt[vars[0]] == arm, vars[1:]].to_numpy(dtype=float).ravel()
        A = theta["A"][k]
        b = theta["b"][k]

        A_inv = np.linalg.inv(A)

        theta_hat = A_inv @ b

        # TODO: compute expected reward for linear UCB
        expected_rewards[k] = None

    best = np.flatnonzero(expected_rewards == expected_rewards.max())
    return context["arms"][int(rng.choice(best))]


# Task 2: build a simulator function for linear UCB.

# %% function for sim

###
# sim_linearucb: simulates performance of a linear UCB policy
#
#   Arguments:
#
#     df: DataFrame with one row per (Date, Symbol)
#
#     vars: list of column names, the first of which identifies the arm
#
#     n_sim: integer, number of observations used to simulate the performance
#
#     c: float, linear UCB penalty parameter
#
#     interval: the number of steps after which our arm is updated.
#
#   Output: dict with the following
#     df_results_of_policy: n_sim x 2 DataFrame, with "arm", "reward"
#     df_sample_of_policy: the data used to evaluate the policy
###
def sim_linearucb(df, vars, n_sim, c, interval=1, seed=0):

    rng = np.random.default_rng(seed)

    # define the number of observations of all data available
    n_obs = len(df)

    # Give user a warning: the size of the intended experiment is bigger than the data provided
    if n_sim > n_obs:
        raise ValueError("The indicated size of the experiment is bigger than the data provided - shrink the size")

    # create dataframe with data that we can sample from during policy
    df_during_policy = df

    # dataframe where the results of the policy are stored
    df_results_policy = pd.DataFrame(np.nan, index=range(n_sim), columns=["arm", "reward"])

    # initialize linear UCB parameters
    d = len(vars) - 1
    arms = np.sort(df[vars[0]].unique())
    n_arms = len(arms)
    context = {"X": df, "d": d, "arms": arms, "n_arms": n_arms}
    theta = {"A": [np.eye(d) for _ in range(n_arms)],
             "b": [np.zeros(d) for _ in range(n_arms)]}

    current_arm = None

    ## part 2: apply linear UCB algorithm, updating at interval
    for i in range(1, n_sim + 1):

        # TODO: choose arm at interval with linear UCB policy
        chosen_arm = None

        # select from the data for experiment the arm chosen
        df_during_policy_arm = df_during_policy[(df_during_policy["Date"] == i)
                                                & (df_during_policy[vars[0]] == chosen_arm)]

        # observe the reward
        if len(df_during_policy_arm) == 0:
            print("You have run out of observations from a chosen arm")
            break

        reward = float(df_during_policy_arm["returns"].iloc[0])

        # add to dataframe to save the result
        df_results_policy.iloc[i - 1] = [chosen_arm, reward]

        # TODO: update regression parameters for chosen_arm

    results = {"df_results_of_policy": df_results_policy,
               "df_sample_of_policy": df}

    return results


# The code below runs 10 simulations for the linear UCB simulator.

# %% run linear UCB

# gather results
n_sim = 2000
n_runs = 10
c = 0.1
vars = ["Symbol", "obv", "roll_returns_week", "prev_returns"]

rng = np.random.default_rng(0)

# rolling origin: run r covers the n_sim days that follow run r-1, so the runs
# are disjoint stretches of time and no day order is altered
step = n_sim

# one column per run
rewards_linUCB = np.zeros((n_sim, n_runs))
for r in range(n_runs):
    first_day = r*step + 1
    df_window = df_coins[df_coins["Date"].between(first_day, first_day + n_sim - 1)].copy()
    df_window["Date"] = df_window["Date"] - first_day + 1
    res = sim_linearucb(df_window, vars, n_sim=n_sim, c=c, interval=1, seed=r)
    rewards_linUCB[:, r] = res["df_results_of_policy"]["reward"].to_numpy()


# Task 3: compare the performance of linear UCB to UCB (without context). Make a plot of the
# cumulative reward (CR) over time, where CR_T = sum over t of log(1 + r_t). Also plot the 95%
# confidence interval. For how many observations should you make the cumulative reward, and which
# of the two algorithms performs best?

# %% ucb policy

def policy_ucb(df, c):

    # get per item, the average reward and the number of items observed
    dfsummary = (df.groupby("arm", as_index=False)
                 .agg(avg_reward=("reward", "mean"), n_pulls=("reward", "size")))

    # the t in this case is simply the total of observations
    t = dfsummary["n_pulls"].sum()

    ucb_reward = dfsummary["avg_reward"] + c*np.sqrt(np.log(t)/dfsummary["n_pulls"])
    return dfsummary["arm"].iloc[int(np.argmax(ucb_reward))]


# %% function for sim_ucb

def sim_ucb(df, n_before_sim, n_sim, c, interval=1, seed=0):

    rng = np.random.default_rng(seed)

    n_obs = len(df)
    if n_sim > (n_obs - n_before_sim):
        raise ValueError("The indicated size of the experiment is bigger than the data provided - shrink the size")

    # find n_before_sim random observations to be used before start policy
    index_before_sim = rng.choice(n_obs, size=n_before_sim, replace=False)
    df_before_policy = df.iloc[index_before_sim]
    df_results_at_t = df_before_policy[["arm", "reward"]].copy().reset_index(drop=True)
    df_during_policy = df.drop(df.index[index_before_sim])

    df_results_policy = pd.DataFrame(np.nan, index=range(n_sim), columns=["arm", "reward"])

    current_arm = None
    for i in range(1, n_sim + 1):

        # update at interval
        if (i == 1) or (i % interval == 0):
            chosen_arm = policy_ucb(df_results_at_t, c)
            current_arm = chosen_arm
        else:
            chosen_arm = current_arm

        # select from the data for experiment the arm chosen
        df_during_policy_arm = df_during_policy[df_during_policy["arm"] == chosen_arm]

        if len(df_during_policy_arm) == 0:
            print("You have run out of observations from a chosen arm")
            break

        # randomly sample from this arm and observe the reward
        sampled_arm = rng.integers(0, len(df_during_policy_arm))
        reward = float(df_during_policy_arm["reward"].iloc[sampled_arm])

        # remove the sampled observation from the dataset to prevent repeated sampling
        index_result = df_during_policy_arm["index"].iloc[sampled_arm]
        df_during_policy = df_during_policy[df_during_policy["index"] != index_result]

        df_results_policy.iloc[i - 1] = [chosen_arm, reward]

        # combine to dataframe with all results
        df_results_at_t = pd.concat(
            [df_results_at_t, pd.DataFrame([{"arm": chosen_arm, "reward": reward}])],
            ignore_index=True)

    return {"df_results_of_policy": df_results_policy,
            "df_sample_of_policy": df_during_policy}


# %% run ucb

# the same rolling windows, so both policies see identical data
rewards_UCB = np.zeros((n_sim, n_runs))
for r in range(n_runs):
    first_day = r*step + 1
    df_window = df_coins[df_coins["Date"].between(first_day, first_day + n_sim - 1)]
    df_coins_UCB = df_window[["Symbol", "returns", "index"]].copy()
    df_coins_UCB.columns = ["arm", "reward", "index"]
    res = sim_ucb(df_coins_UCB, n_before_sim=100, n_sim=n_sim, c=c, interval=1, seed=r)
    rewards_UCB[:, r] = res["df_results_of_policy"]["reward"].to_numpy()


# %% compare the two policies

# TODO: add plot of the average cumulative rewards for all simulations, together with the 95%
# confidence interval. Do this for both UCB (without context) and linear UCB.


# Your answer to Task 3 here:

# (Bonus) Task 4: can you make the linear UCB algorithm even better by adding context? Create a
# variable of your own, and assess if it makes the algorithm perform better. Explain why you added
# the variable.
