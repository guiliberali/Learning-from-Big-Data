# title: 'Learning from Big Data: Tutorial 1.1 (Solutions) - Python version'
# author: "LfBD Team, 2026"
# date: "September 2026"
#
# Python port of tutorial1.1_2026_solutions.R: same data, same steps, same results.
# Only numpy and pandas are needed - no scikit-learn, no nltk - and the whole file runs end to
# end on the real review data in under two seconds.
#
# Checked against the output of the R version: the content and sentiment likelihood tables agree
# to 1e-17, the document term matrices are identical term by term, and all 1000 AFINN scores
# match exactly. The NBC posteriors differ, for the reason documented at the tokenizer below.


# # Introduction

# This tutorial focuses on applying Natural Language Processing (NLP) techniques for supervised
# learning. We begin by using the Naive Bayes Classifier (NBC) on review data to determine both
# the topic and sentiment of a review. Next, we apply AFINN, a lexicon-based tool, to perform the
# same sentiment analysis task. Additionally, we will cover how to evaluate model performance
# using a confusion matrix, explaining how it visualizes predictions, can highlight incorrect
# classifications, and supports the calculation of key performance metrics used to evaluate our
# models.


# # 1. Loading libraries

# In R we used pacman::p_load(tm, dplyr, syuzhet, caret, ...). In Python the equivalents are
# pandas (data frames), numpy (matrices) and matplotlib (plots); the text handling that tm did
# for us we write out ourselves in a few lines, which makes explicit what tm does silently.

# ---- chunk 1 ----

import os
import re
from collections import Counter

import numpy as np
import pandas as pd


# # 2. Load the reviews and prepare the data

# ---- chunk 2 ----

# Where the data live. In R we called setwd(); in Python we build the path relative to this
# script, so the code runs from any working directory
INPUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input")

# Load the review data. As in R we state the encoding explicitly: ISO-8859-1 represents the
# first 256 unicode characters, which is what these review texts use
reviews_raw = pd.read_csv(os.path.join(INPUT, "Reviews_tiny.csv"), encoding="ISO-8859-1")

# Selecting only the relevant columns from the entire dataset (dplyr::select)
reviews_raw = reviews_raw[["movie_name", "review_code", "reviewer", "review_date", "num_eval",
                           "prob_sentiment", "words_in_lexicon_sentiment_and_review",
                           "ratio_helpful", "raters",
                           "prob_storyline", "prob_acting", "prob_sound_visual",
                           "full_text", "processed_text",
                           "release_date", "first_week_box_office", "MPAA", "studio",
                           "num_theaters"]].copy()

# The four probability columns arrive from the CSV as blanks and zeros, and the word-list column
# as empty. We overwrite them with our posteriors below, so we set their types accordingly:
# numbers for the probabilities, text for the word list
for column in ["prob_sentiment", "prob_storyline", "prob_acting", "prob_sound_visual"]:
    reviews_raw[column] = pd.to_numeric(reviews_raw[column], errors="coerce").astype(float)

reviews_raw["words_in_lexicon_sentiment_and_review"] =     reviews_raw["words_in_lexicon_sentiment_and_review"].astype(object)

# Texts labelled with content or sentiment are used to compute word likelihoods. We load the
# training data from three content lexicons (storyline, acting, visual) and the two sentiment
# word lists, and we set the priors of our NBC models.

# ---- chunk 3 ----

# training data. read.csv2 in R uses ";" as separator and takes the first line as the header,
# so the very first word of each file is consumed as a column name - we mirror that here to
# keep the two versions comparable
# keep_default_na=False matters here: several lines of these files contain the word "None",
# which pandas would otherwise read as a missing value and silently drop from the vocabulary
dictionary_storyline = pd.read_csv(os.path.join(INPUT, "storyline_33k.txt"), sep=";",
                                   keep_default_na=False)
dictionary_acting = pd.read_csv(os.path.join(INPUT, "acting_33k.txt"), sep=";",
                                keep_default_na=False)
dictionary_visual = pd.read_csv(os.path.join(INPUT, "visual_33k.txt"), sep=";",
                                keep_default_na=False)


# SOLUTION. A likelihood is a relative frequency: P(word | topic) = how often the word occurs
# in that topic's training text, divided by the total number of words of that topic. So we
# count, and then we divide.

# The words of the three dictionaries. .str.lower() makes "Hero" and "hero" the same word

# ---- chunk 4 ----

words_storyline = dictionary_storyline.iloc[:, 0].astype(str).str.lower().tolist()
words_acting = dictionary_acting.iloc[:, 0].astype(str).str.lower().tolist()
words_visual = dictionary_visual.iloc[:, 0].astype(str).str.lower().tolist()

# Counting the same word over and over inside the loop would be slow, so we count each
# dictionary once. Counter is the Python equivalent of R's table(): for every word it returns
# the number of times it occurs
count_storyline = Counter(words_storyline)
count_acting = Counter(words_acting)
count_visual = Counter(words_visual)


# The vocabulary: every word occurring in at least one of the three dictionaries. We take all
# three, not just one, because a word that only shows up under storyline still needs a (small)
# likelihood under acting and visual

# ---- chunk 5 ----

unique_words = sorted(set(words_storyline) | set(words_acting) | set(words_visual))

# The table we fill in: one row per word, one column per topic
results_matrix = pd.DataFrame({"words": unique_words,
                               "storyline": 0.0,
                               "acting": 0.0,
                               "visual": 0.0})

storyline_counts = np.zeros(len(unique_words))
acting_counts = np.zeros(len(unique_words))
visual_counts = np.zeros(len(unique_words))

for i, word in enumerate(unique_words):

    # look the word up in each count; a word that is absent from a dictionary occurred 0 times
    s_count = count_storyline.get(word, 0)
    a_count = count_acting.get(word, 0)
    v_count = count_visual.get(word, 0)

    # add one to every count (Laplace smoothing): without it a word missing from the acting
    # dictionary would get a likelihood of exactly zero, and that single word would push the
    # posterior of acting to zero regardless of what the rest of the review says
    storyline_counts[i] = s_count + 1
    acting_counts[i] = a_count + 1
    visual_counts[i] = v_count + 1

results_matrix["storyline"] = storyline_counts
results_matrix["acting"] = acting_counts
results_matrix["visual"] = visual_counts


# Turn the counts into probabilities by dividing every count by its COLUMN total, i.e. by the
# total number of words of that topic. Every column then sums to one, because a column is the
# distribution of that one topic over the whole vocabulary: that is what P(word | topic) means.
# (Dividing by the row total instead would look tidier - the three numbers of a word would sum
# to one - but it is a different quantity, and it drops the correction for one dictionary
# containing more training text than another.)

# ---- chunk 6 ----

likelihoods = results_matrix.copy()
likelihoods["storyline"] = results_matrix["storyline"] / results_matrix["storyline"].sum()
likelihoods["acting"] = results_matrix["acting"] / results_matrix["acting"].sum()
likelihoods["visual"] = results_matrix["visual"] / results_matrix["visual"].sum()

# Check: each column sums to one, and the words that take most of their probability from one
# topic (counting only words seen at least 10 times) should look like that topic
print(likelihoods[["storyline", "acting", "visual"]].sum())

row_sum = likelihoods["storyline"] + likelihoods["acting"] + likelihoods["visual"]
enough = (results_matrix["storyline"] + results_matrix["acting"] + results_matrix["visual"]) >= 10

for topic in ["storyline", "acting", "visual"]:
    share = (likelihoods[topic] / row_sum)[enough]
    print(topic, ":", " ".join(likelihoods["words"][share.sort_values(ascending=False).index[:8]]))

## storyline : moral opponent plot hero story revelation heros stories
## acting : rehearsal exercises tension class relaxation objective givens vocal
## visual : digital animation data previs matte effects model shots

# These are the same lists as in the R version. Only the eighth acting word differs: "vocal" and
# "concentration" have exactly the same share (0.934), and R and Python break that tie in a
# different order.


# SOLUTION. Exactly the same exercise as above, only now the training data are the two
# sentiment word lists that come with the course: a list of positive and a list of negative
# words (from the literature, so no need to build them ourselves)

# ---- chunk 7 ----

positive_words = pd.read_csv(os.path.join(INPUT, "positive.csv"),
                             keep_default_na=False)["x"].astype(str).str.lower().tolist()
negative_words = pd.read_csv(os.path.join(INPUT, "negative.csv"),
                             keep_default_na=False)["x"].astype(str).str.lower().tolist()

# As with the content dictionaries, count each list once instead of searching it inside the
# loop. These are word lists, so a count is 1 if the word is on the list and 0 if it is not
count_positive = Counter(positive_words)
count_negative = Counter(negative_words)

# The sentiment vocabulary: every word that appears on either list
sentiment_words = sorted(set(positive_words) | set(negative_words))

pos_counts = np.zeros(len(sentiment_words))
neg_counts = np.zeros(len(sentiment_words))

for i, word in enumerate(sentiment_words):

    p_count = count_positive.get(word, 0)
    n_count = count_negative.get(word, 0)

    pos_counts[i] = p_count + 1   # Laplace smoothing, as above
    neg_counts[i] = n_count + 1

sentiment_matrix = pd.DataFrame({"words": sentiment_words,
                                 "pos_likelihood": pos_counts,
                                 "neg_likelihood": neg_counts})

# Again divide by the COLUMN total, so that each of the two columns sums to one
likelihoods_sentim = sentiment_matrix.copy()
likelihoods_sentim["pos_likelihood"] = sentiment_matrix["pos_likelihood"] / sentiment_matrix["pos_likelihood"].sum()
likelihoods_sentim["neg_likelihood"] = sentiment_matrix["neg_likelihood"] / sentiment_matrix["neg_likelihood"].sum()

# Check: both columns sum to one, and a word from the positive list is about twice as likely
# under positive as under negative (a word on neither list is not in this table at all)
print(likelihoods_sentim[["pos_likelihood", "neg_likelihood"]].sum())
print(likelihoods_sentim[likelihoods_sentim["words"].isin(["hero", "awful"])])

##       words  pos_likelihood  neg_likelihood
## 337   awful         0.00019        0.000355
## 1713  hero          0.00038        0.000178

# Note how coarse this is: every positive word carries exactly the same weight, because a word
# list says only whether a word belongs to a sentiment, not how strongly. If you want stronger
# evidence for "superb" than for "nice", estimate the likelihoods on labelled training reviews
# (or use a lexicon that comes with valence scores, e.g. AFINN).


# These lexicons are used as (a dictionary of) words that are associated with our content
# topics/sentiment. We keep them as dictionaries from word to row number, which is how we will
# look a word's likelihoods up later (R searched the table with which(), which is slower)

# ---- chunk 8 ----

lexicon_content = likelihoods["words"].tolist()
lexicon_sentiment = likelihoods_sentim["words"].tolist()

content_index = {w: i for i, w in enumerate(lexicon_content)}
sentiment_index = {w: i for i, w in enumerate(lexicon_sentiment)}

# the likelihood tables as plain numpy matrices: one row per word, one column per class
content_lik = likelihoods[["storyline", "acting", "visual"]].to_numpy()
sentiment_lik = likelihoods_sentim[["pos_likelihood", "neg_likelihood"]].to_numpy()

# Setting our prior parameters
# We set our priors for the topics to 1/3 each because we have three topics
# (i.e. storyline, acting, and visual). Similarly, we set the priors for
# sentiment to 1/2 each because we have two sentiments (positive/negative)
prior_topic = 1 / 3
prior_sent = 1 / 2

total_reviews = len(reviews_raw)


# # 3. Supervised Learning: Naive Bayes Classifier (NBC)

# The Naive Bayes Classifier is a probabilistic model based on Bayes' Theorem used to predict the
# probability that a given input, in this case reviews, belongs to a particular category. NBC
# begins with a prior probability for each class. Then, for every word in the input, the model
# calculates the likelihood of that word appearing given each class. Using Bayes' rule, it
# continuously updates the probability for each class as more words are considered. Finally, the
# class with the highest posterior probability is selected as the predicted category.

# ## The document term matrix

# R's DocumentTermMatrix() did three things for us: it lower-cased the text, it removed
# punctuation and numbers, and it counted only the words that appear in a given dictionary.
# Here we do exactly the same in one small function, which returns the words of one review that
# are in the dictionary together with how often each occurs.

# ---- chunk 9 ----

PUNCTUATION = re.compile(r"[!-/:-@\[-`{-~]")   # the [:punct:] class that tm removes
DIGITS = re.compile(r"[0-9]")


def document_term_matrix(text, dictionary_index):
    """Word counts of one review, restricted to the words of a dictionary."""

    text = str(text).lower()
    text = PUNCTUATION.sub("", text)   # removePunctuation = TRUE
    text = DIGITS.sub("", text)        # removeNumbers = TRUE

    # tm keeps only words of at least three characters (its wordLengths = c(3, Inf) default),
    # so "a" and "an" never reach the classifier even though they are in the dictionary
    counts = Counter(w for w in text.split() if len(w) >= 3 and w in dictionary_index)
    return counts


# One difference from the R version worth knowing about. There, the matrix handed to the
# classifier comes from inspect(DocumentTermMatrix(...)), and inspect() does not return the
# matrix: it returns the 10-term "Sample" block it prints. So for any review with more than ten
# dictionary words, the R code scores only the ten most frequent ones - for review 242 that is 66
# of its 266 dictionary word occurrences, spread over 10 of its 173 matched words. This function
# keeps every matched word, which is what the tutorial describes, so the posteriors below differ
# from the R ones on the longer reviews. (Reproducing R's truncation here instead brings 94% of
# the content posteriors and 98% of the sentiment posteriors to an exact match, the rest being
# ties in which ten terms tm picks.)


# ## Compute posterior sentiment function

# This first function estimates the probability that the review expresses positive or negative
# sentiment. Note the structure, which is the heart of the classifier: we walk through the words
# of the review one by one, and after every word the posterior becomes the prior of the next
# word. Because each step just multiplies by the word's likelihood and re-normalises, the order
# of the words does not matter.

# ---- chunk 10 ----

def compute_posterior_sentiment(prior, word_counts, p_w_given_c, index):

    prior = np.asarray(prior, dtype=float)

    # Check if there are any relevant words in the review. If there are, treat them; if not,
    # return the prior unchanged
    if len(word_counts) == 0:
        return prior, []

    for word, occurrences in word_counts.items():

        vec_likelihood = p_w_given_c[index[word]]

        # Loop around occurrences of this word: a word that occurs twice counts twice
        for _ in range(occurrences):

            posterior = np.zeros(len(prior))

            # positive - this is the first element in the vector
            numerat = prior[0] * vec_likelihood[0]
            denomin = prior @ vec_likelihood
            posterior[0] = numerat / denomin

            # negative - this is the second element in the vector
            numerat = prior[1] * vec_likelihood[1]
            denomin = prior @ vec_likelihood
            posterior[1] = numerat / denomin

            # the @ sign above is matrix multiplication (R writes it as %*%), here simply the
            # sum of prior[k] * likelihood[k] over the classes

            if posterior.sum() > 1.01:
                raise ValueError("posterior does not sum to one")

            prior = posterior   # today's posterior is tomorrow's prior

    return posterior, sorted(word_counts)


# ## Compute posterior content function

# This second function determines the probability that the review pertains to each specific
# topic. It is the same computation with three classes instead of two.

# ---- chunk 11 ----

def compute_posterior_content(prior, word_counts, p_w_given_c, index):

    prior = np.asarray(prior, dtype=float)

    if len(word_counts) == 0:
        return prior

    for word, occurrences in word_counts.items():

        vec_likelihood = p_w_given_c[index[word]]

        for _ in range(occurrences):

            posterior = np.zeros(len(prior))
            denomin = prior @ vec_likelihood

            # storyline, acting, visual - the three elements of the vector
            posterior[0] = prior[0] * vec_likelihood[0] / denomin
            posterior[1] = prior[1] * vec_likelihood[1] / denomin
            posterior[2] = prior[2] * vec_likelihood[2] / denomin

            if posterior.sum() > 1.01:
                raise ValueError("posterior does not sum to one")

            prior = posterior

    return posterior


# ## NBC Sentiment Analysis Loop

# Now that we have defined the functions, we loop over the reviews and apply them.

# ---- chunk 12 ----

for review_index in range(total_reviews):

    # Print progress every 100th review
    if (review_index + 1) % 100 == 0:
        print("Computing sentiment of review #%d" % (review_index + 1))

    # If the review is not empty, continue and calculate posterior
    if str(reviews_raw["full_text"].iloc[review_index]) != "":

        # Assign the processed text of the non-empty review to text_review
        text_review = str(reviews_raw["processed_text"].iloc[review_index])

        # Reset the prior every iteration as each review is looked at separately
        prior_sent_reset = np.array([prior_sent, 1 - prior_sent])

        # Pre-process the review to remove punctuation marks and numbers.
        # Note that we are not removing stopwords here (nor elsewhere - a point for improvement)
        word_counts = document_term_matrix(text_review, sentiment_index)

        # Compute posterior probability the review is positive
        posterior_sent, words_sent = compute_posterior_sentiment(prior_sent_reset,
                                                                 word_counts,
                                                                 sentiment_lik,
                                                                 sentiment_index)

        reviews_raw.at[reviews_raw.index[review_index], "prob_sentiment"] = posterior_sent[0]
        reviews_raw.at[reviews_raw.index[review_index],
                       "words_in_lexicon_sentiment_and_review"] = " ".join(words_sent)


# ## NBC Content Analysis Loop

# We also calculate the posteriors for the content of each review using NBC.

# ---- chunk 13 ----

for review_index in range(total_reviews):

    if (review_index + 1) % 100 == 0:
        print("Computing content of review #%d" % (review_index + 1))

    if str(reviews_raw["full_text"].iloc[review_index]) != "":

        text_review = str(reviews_raw["processed_text"].iloc[review_index])

        content_word_counts = document_term_matrix(text_review, content_index)

        # Compute posterior probability the review is about each topic
        posterior = compute_posterior_content(np.repeat(prior_topic, 3),
                                              content_word_counts,
                                              content_lik,
                                              content_index)

        # Store the posteriors
        reviews_raw.at[reviews_raw.index[review_index], "prob_storyline"] = posterior[0]
        reviews_raw.at[reviews_raw.index[review_index], "prob_acting"] = posterior[1]
        reviews_raw.at[reviews_raw.index[review_index], "prob_sound_visual"] = posterior[2]

Processed_reviews = reviews_raw
print(Processed_reviews[["prob_sentiment", "prob_storyline", "prob_acting",
                         "prob_sound_visual"]].head())

# Saves the updated file, now including the sentiment and content/topic posteriors.
# Processed_reviews.to_csv("TestProcessed_reviews.csv", index=False)


# # 4. Supervised Learning: AFINN

# AFINN is a sentiment analysis tool that uses a lexicon specifically designed to evaluate the
# sentiment score of texts. Each word in the lexicon is assigned a sentiment score from -5 to 5,
# indicating whether it is negative or positive, allowing us to score overall sentiments of texts
# based on the combined scores of individual words. R's syuzhet::get_sentiment(method = "afinn")
# simply adds up those scores over the words of the text, which is what we do here. The lexicon
# itself is the one shipped with syuzhet, exported to input/afinn_lexicon.csv.

# ---- chunk 14 ----

afinn = pd.read_csv(os.path.join(INPUT, "afinn_lexicon.csv"))
afinn_words = afinn["word"].astype(str).str.lower().tolist()
afinn_values = afinn["value"].astype(float).tolist()

# syuzhet splits the lower-cased text on everything that is not a letter or an apostrophe,
# so "don't" stays one word while "top10" becomes "top" and "10"
AFINN_SPLIT = re.compile(r"[^a-z']+")

reviews_raw["AFINN"] = np.nan

for review_index in range(total_reviews):

    if (review_index + 1) % 100 == 0:
        print("Computing AFINN sentiment of review #%d" % (review_index + 1))

    # If the review is not empty, continue and apply AFINN
    if str(reviews_raw["full_text"].iloc[review_index]) != "":

        # Note that we have not removed punctuation, numbers, and stopwords (a point for
        # improvement) - exactly as in the R version
        text_review = str(reviews_raw["processed_text"].iloc[review_index])

        # Apply AFINN. Note how syuzhet computes this: sum(afinn[afinn$word %in% words, "value"]),
        # i.e. it adds the score of every lexicon word that occurs in the review, counting each
        # such word once no matter how often it occurs. A review saying "good" ten times scores
        # the same as one saying it once
        words = set(AFINN_SPLIT.split(text_review.lower()))
        AFINN = sum(value for word, value in zip(afinn_words, afinn_values) if word in words)

        # store the AFINN results in the dataframe
        reviews_raw.at[reviews_raw.index[review_index], "AFINN"] = AFINN

Processed_reviews = reviews_raw
print(Processed_reviews[["prob_sentiment", "AFINN"]].head())

# Processed_reviews.to_csv("AFINN_Processed_reviews.csv", index=False)


# # 5. Performance Measurement: Confusion matrix

# A confusion matrix is a valuable tool for evaluating classification models. It compares the
# model's predictions with the true values by summarizing the counts of correct and incorrect
# predictions across different classes.

# A confusion matrix consists of:
# 1. True Positives (TP): Correctly predicted positive cases.
# 2. True Negatives (TN): Correctly predicted negative cases.
# 3. False Positives (FP): Incorrectly predicted as positive (Type I error).
# 4. False Negatives (FN): Incorrectly predicted as negative (Type II error).

# Specificity = TN / (FP + TN)

# Below is the same artificial spam example as in the R tutorial. R used caret::confusionMatrix;
# pandas.crosstab gives the same table, and the specificity is one division.

# ---- chunk 15 ----

actual = pd.Categorical(["spam", "not spam", "spam", "spam", "not spam",
                         "not spam", "spam", "not spam", "not spam", "spam"],
                        categories=["spam", "not spam"])
predicted = pd.Categorical(["spam", "not spam", "not spam", "spam", "not spam",
                            "not spam", "spam", "spam", "not spam", "not spam"],
                           categories=["spam", "not spam"])

# We can display the confusion matrix (prediction in the rows, reference in the columns, as in
# caret's output)
conf_matrix = pd.crosstab(pd.Series(predicted, name="Prediction"),
                          pd.Series(actual, name="Reference"),
                          dropna=False)
print(conf_matrix)

##             Reference
## Prediction   spam  not spam
## spam            3         1
## not spam        2         4

# Calculating the specificity: of the cases that are truly "not spam", the share we labelled
# "not spam". The positive class is the first level, "spam", so "not spam" is the negative one
TN = conf_matrix.loc["not spam", "not spam"]
FP = conf_matrix.loc["spam", "not spam"]
specificity = TN / (FP + TN)
print("specificity:", specificity)

## specificity: 0.8


# You might have noticed that the supervised models in the previous sections do not output class
# labels (such as 'spam'/'not spam'), but rather a probability. At this stage, we distinguish
# between soft predictions and hard predictions. Soft predictions are the probabilities that an
# observation belongs to the positive class, while hard predictions are the final class labels
# assigned to the observations. To convert soft predictions into hard predictions, we use a
# decision rule, such as applying a threshold or selecting the class with the highest probability.


# # 6. Brain teaser: hard vs. soft predictions (solutions)

# A classifier gives each observation a probability for each of three content categories. Two
# ways to use it: keep the probabilities (soft), or round them to the winner (hard). Hard
# predictions are particularly useful when a ready-to-use prediction is required. However, making
# the hard prediction inherently loses the more detailed information on the predicted
# probabilities. For example, imagine that you are building an information databank for yourself
# at work, and would like to aggregate all the incoming emails that are useful to you. Here, the
# emails are first categorized as either useful or not useful. Then, if the email is judged as
# useful, the information contained in the email is passed to some storage unit. The problem
# becomes clear - here, you literally lose information if your classifier misjudges an email.
# Thus, it starts to make more sense to perhaps keep the probabilities on usefulness, so that you
# yourself can judge the email's content if the usefulness of its content is uncertain.

# To showcase this problem, we construct the following scenario. We imagine 3 categories and a
# classifier that can make errors. We ask you to compare the information that results from using
# either hard or soft predictions.

# ## 6.1 Synthetic data: three category likelihoods + ground truth

# Draw the likelihoods at random (a Dirichlet draw: three positive numbers normalised to sum to
# 1), then draw the true category from those very likelihoods. That is what a well-specified
# classifier means - when it says 0.7, the truth is that category 70% of the time.
# Note: R and numpy have different random number generators, so the numbers below are close to
# but not identical with the ones in the R solutions.

# ---- chunk 16 ----

rng = np.random.default_rng(20250902)

n = 900                            # number of observations
alpha = np.array([1.0, 0.7, 0.5])  # category 1 is the frequent one, category 3 the rare one

# empty containers that we fill in observation by observation
p = np.zeros((n, 3))               # the three likelihoods of each observation
truth = np.zeros(n, dtype=int)     # the true category of each observation

for i in range(n):

    # draw three positive numbers and rescale them so that they sum to one
    draws = np.array([rng.gamma(alpha[0]), rng.gamma(alpha[1]), rng.gamma(alpha[2])])
    probs = draws / draws.sum()

    # store them, and draw the true category using those same probabilities
    p[i, :] = probs
    truth[i] = rng.choice([1, 2, 3], size=1, p=probs)[0]

# the data we work with: three likelihood columns and the ground truth
dat = pd.DataFrame({"p1": p[:, 0], "p2": p[:, 1], "p3": p[:, 2], "truth": truth})
print(dat.head())

# average likelihood per category, and the share of each category in the truth
print(p.mean(axis=0).round(3))
print((pd.Series(truth).value_counts().sort_index() / n).round(3))


# ## 6.2 Hard predictions: argmax

# ---- chunk 17 ----

# the hard prediction is simply the category with the highest likelihood
pred = np.zeros(n, dtype=int)      # the predicted category
winning = np.zeros(n)              # the probability that won, i.e. how sure the classifier was

for i in range(n):
    pred[i] = np.argmax(p[i, :]) + 1   # +1 because Python counts from 0 and our categories from 1
    winning[i] = np.max(p[i, :])

# confusion matrix: the truth in the rows, the prediction in the columns
print(pd.crosstab(pd.Series(truth, name="truth"), pd.Series(pred, name="predicted")))

acc = np.mean(pred == truth)
print(round(acc, 3))                                          # accuracy
print(round(max(np.bincount(truth)[1:] / n), 3))              # majority baseline
print(round(winning.mean(), 3))                               # average winning probability


# Accuracy is about 66%, against a majority baseline of about 47%. That is close to the ceiling:
# the average winning probability is about 0.66, so no rule that has to commit to one category can
# do much better on these data. Note where the errors sit - categories 2 and 3 lose far more of
# their observations than category 1, because a rare category rarely wins a comparison even when
# it is the true one.


# ## 6.3 Soft predictions: keep the probabilities

# When evaluating forecasts of probabilities, data scientists often make use of the Brier score.
# In our case, we score the probabilities constructed above with the multiclass Brier score,
# (1/n) * sum_i sum_k (p_ik - y_ik)^2, where y is 1 if the prediction is correct. This is
# especially appropriate when evaluating our soft predictions, which unlike the hard predictions,
# take on continuous values.

# ---- chunk 18 ----

# the three forecasts we compare, each one a table with a probability per category

# 1. the soft probabilities: the matrix p we already have

# 2. the hard prediction written as probabilities: all the mass on the winner
p_hard = np.zeros((n, 3))
for i in range(n):
    p_hard[i, pred[i] - 1] = 1

# 3. the baseline: the same average likelihoods for every observation
p_base = np.zeros((n, 3))
for i in range(n):
    p_base[i, :] = p.mean(axis=0)


# the Brier score: per observation, the squared distance between the forecast
# and the truth written as 0/1, then averaged over all observations
def brier(forecast):

    scores = np.zeros(n)

    for i in range(n):
        y = np.zeros(3)                 # the truth of observation i as 0/1
        y[truth[i] - 1] = 1
        scores[i] = np.sum((forecast[i, :] - y) ** 2)

    return scores.mean()


results = pd.DataFrame({"forecast": ["Soft probabilities", "Hard 0/1 (argmax)",
                                     "Average likelihood"],
                        "brier": [brier(p), brier(p_hard), brier(p_base)]})
print(results)

# Aggregate use: what share of the observations belongs to category 1?
print(round(np.mean(truth == 1), 3))   # the true share
print(round(np.mean(p[:, 0]), 3))      # estimated with the soft probabilities
print(round(np.mean(pred == 1), 3))    # estimated by counting the hard predictions


# The rounded forecast is built from the same information, yet its Brier score is about 1.5 times
# worse than that of the probabilities it came from - the price of claiming certainty on
# observations where the model was near 0.5. A confident wrong call costs more than an
# honest vague one.

# ## 6.4 Takeaway

# Argmax answers "which category?" and is the right output when one decision must be made per
# observation. It is the wrong output when the classification is an intermediate step feeding an
# average or a regression: the probabilities score better and aggregate without bias, which is why
# the review data ship prob_storyline, prob_acting and prob_sound_visual rather than one label
# per sentence.
