# title: 'Learning from Big Data: Tutorial 1.2 (Solutions) - Python version'
# author: "LfBD Team, 2026"
# date: "September 2026"
#
# Python port of tutorial1.2_2026_solutions.R. Same data, same steps.
# Only numpy / pandas / statsmodels / matplotlib are needed: the two models of this tutorial
# (LDA with Gibbs sampling, and word2vec with CBOW) are written out here in about forty lines
# each, in place of R's topicmodels::LDA() and word2vec::word2vec(). Runtime is a few minutes,
# almost all of it in those two samplers.


# # Introduction

# In this tutorial, we explore unsupervised learning techniques used in NLP, which do not require
# labeled data. Specifically, we will cover Latent Dirichlet Allocation (LDA), a topic modeling
# method used to uncover abstract topics within text, and Word2Vec (W2V), which is a technique for
# embedding words into vectors. These methods will be applied to extract insights from review
# data. We will also take initial steps toward using Word2Vec embeddings in a predictive model to
# forecast movie box office performance.


# # 1. Loading libraries

# ---- chunk 1 ----

import os
import re
import time
from collections import Counter

import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.api as sm


# # 2. Load the reviews

# ---- chunk 2 ----

# Where the data live. In R the tutorial expects the working directory to be tutorials/input;
# here we build that path relative to this script, so it runs from anywhere
INPUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "input")

# Load the review data. The ISO-8859-1 encoding represents the first 256 unicode characters,
# which is what these review texts use
reviews_raw = pd.read_csv(os.path.join(INPUT, "Reviews_tiny.csv"), encoding="ISO-8859-1")

# Selecting only the relevant columns from the entire dataset
reviews_raw = reviews_raw[["movie_name", "review_code", "reviewer", "review_date", "num_eval",
                           "prob_sentiment", "words_in_lexicon_sentiment_and_review",
                           "ratio_helpful", "raters",
                           "prob_storyline", "prob_acting", "prob_sound_visual",
                           "full_text", "processed_text",
                           "release_date", "first_week_box_office", "MPAA", "studio",
                           "num_theaters"]].copy()

# Determining the total number of reviews in our dataset
total_reviews = len(reviews_raw)

# Loading fake likelihoods data
likelihoods = pd.read_csv(os.path.join(INPUT, "example_100_fake_likelihood_topic.csv"))


# Inspect list of words to be passed to LDA:

# ---- chunk 3 ----

# set our lexicon equal to the first column of the likelihoods data and inspect its structure
lexicon_content = likelihoods.iloc[:, 0].astype(str).str.lower().tolist()
print("lexicon_content: %d words, e.g. %s" % (len(lexicon_content), lexicon_content[:6]))


# # 3. Unsupervised Learning: Latent Dirichlet Allocation (LDA)

# Latent Dirichlet Allocation (LDA) is an unsupervised learning technique used for discovering
# topics within a collection of documents or texts. We apply this topic modeling technique in this
# context to get an understanding of the main topics people are talking about in their reviews.

# ---- chunk 4 ----

# Put processed reviews in corpus format and create the document term matrix that will be passed
# to LDA. R's DocumentTermMatrix() lower-cased the text, removed punctuation and numbers and kept
# only the words of our dictionary; here we do the same explicitly.
PUNCTUATION = re.compile(r"[!-/:-@\[-`{-~]")   # the [:punct:] class that tm removes
DIGITS = re.compile(r"[0-9]")

vocab_index = {word: j for j, word in enumerate(lexicon_content)}

dtm = np.zeros((total_reviews, len(lexicon_content)), dtype=int)

for i, text in enumerate(reviews_raw["processed_text"]):

    text = str(text).lower()
    text = PUNCTUATION.sub("", text)   # removePunctuation = TRUE
    text = DIGITS.sub("", text)        # removeNumbers = TRUE

    for word, count in Counter(text.split()).items():
        # tm keeps only words of at least three characters (wordLengths = c(3, Inf))
        if len(word) < 3:
            continue
        j = vocab_index.get(word)
        if j is not None:
            dtm[i, j] = count

# Inspecting the structure of the document term matrix
print("dtm shape:", dtm.shape,
      " non-zero cells:", int((dtm > 0).sum()),
      " total word occurrences:", int(dtm.sum()))


# Next, we will set the LDA parameters. k is the number of topics we ask LDA to estimate. In
# supervised learning, we set that to 3. In this example, we arbitrarily set k at 10 to obtain 10
# topics. Seed is for replicability. Burn-in and number of iterations are for the convergence of
# the Markov chains in the Gibbs sampler (MCMC-based inference is outside of the scope of this
# course and not required for the assignment - just use the default values below).

# ---- chunk 5 ----

# LDA parameters
seed = 2
burnin = 2000
iter_ = 1000
k = 10

# The two Dirichlet priors. These are the defaults of R's topicmodels for Gibbs sampling:
# alpha over the topics of a document, delta over the words of a topic
alpha = 50 / k
delta = 0.1


# Next we run the LDA. R called topicmodels::LDA(dtm, k, method = "Gibbs"). The collapsed Gibbs
# sampler it uses fits in the loop below: we walk through every word occurrence of every review
# and re-draw which topic it belongs to, given the topics of all other words. After enough sweeps
# the counts we accumulate describe the topics. Three counters are all the sampler needs:
#   n_dk[d, t] - how many words of review d currently belong to topic t
#   n_tw[t, w] - how often word w is currently assigned to topic t
#   n_t[t]     - how many words in total belong to topic t

# ---- chunk 6 ----

def lda_gibbs(dtm, k, alpha, delta, burnin, iter_, seed):

    n_docs, n_words = dtm.shape

    # flatten the document term matrix into one long list of word occurrences
    doc_of_token, word_of_token = [], []
    for d in range(n_docs):
        for w in np.nonzero(dtm[d])[0]:
            doc_of_token.extend([d] * dtm[d, w])
            word_of_token.extend([w] * dtm[d, w])

    n_tokens = len(doc_of_token)
    rng = np.random.default_rng(seed)

    # start from a random assignment
    topic_of_token = [int(t) for t in rng.integers(0, k, size=n_tokens)]

    n_dk = [[0.0] * k for _ in range(n_docs)]
    n_tw = [[0.0] * n_words for _ in range(k)]
    n_t = [0.0] * k

    for i in range(n_tokens):
        d, w, t = doc_of_token[i], word_of_token[i], topic_of_token[i]
        n_dk[d][t] += 1
        n_tw[t][w] += 1
        n_t[t] += 1

    # the counts we average over after the burn-in
    n_dk_sum = np.zeros((n_docs, k))
    n_tw_sum = np.zeros((k, n_words))
    n_samples = 0

    delta_total = n_words * delta
    started = time.time()

    for sweep in range(burnin + iter_):

        if (sweep + 1) % 250 == 0:
            print("  Gibbs sweep %d of %d (%.0f s)"
                  % (sweep + 1, burnin + iter_, time.time() - started))

        for i in range(n_tokens):

            d = doc_of_token[i]
            w = word_of_token[i]
            t = topic_of_token[i]
            row = n_dk[d]

            # take this word occurrence out of the counts
            row[t] -= 1
            n_tw[t][w] -= 1
            n_t[t] -= 1

            # probability of each topic for this word: how much this review likes the topic,
            # times how much the topic likes this word
            cumulative = 0.0
            p = [0.0] * k
            for topic in range(k):
                cumulative += ((row[topic] + alpha)
                               * (n_tw[topic][w] + delta) / (n_t[topic] + delta_total))
                p[topic] = cumulative

            # draw a topic from that distribution
            u = rng.random() * cumulative
            topic = 0
            while p[topic] < u:
                topic += 1

            # and put the word occurrence back in, under its new topic
            topic_of_token[i] = topic
            row[topic] += 1
            n_tw[topic][w] += 1
            n_t[topic] += 1

        # after the burn-in, keep the counts of this sweep
        if sweep >= burnin:
            n_dk_sum += np.array(n_dk)
            n_tw_sum += np.array(n_tw)
            n_samples += 1

    # turn the averaged counts into the two posterior distributions
    topics = (n_dk_sum / n_samples) + alpha
    topics = topics / topics.sum(axis=1, keepdims=True)          # P(topic | document)

    terms = (n_tw_sum / n_samples) + delta
    terms = terms / terms.sum(axis=1, keepdims=True)             # P(word | topic)

    return topics, terms


print("Fitting LDA with Gibbs sampling (%d sweeps) - this takes a couple of minutes"
      % (burnin + iter_))
posteriors_lda, terms_lda = lda_gibbs(dtm, k, alpha, delta, burnin, iter_, seed)

# R saved the fitted model with save(model_lda, file = "LDA_model_10.RData"); numpy's
# equivalent for a couple of arrays is savez
np.savez(os.path.join(INPUT, "LDA_model_%d.npz" % k),
         posteriors=posteriors_lda, terms=terms_lda)


# Inspect posteriors.

# ---- chunk 7 ----

# posterior probabilities per document by topic
print("posteriors_lda shape:", posteriors_lda.shape)
# the same row the R version prints with posteriors_lda[999, ]; Python counts from 0, so
# review 999 sits at index 998
print("review 999, topic probabilities:")
print(np.round(posteriors_lda[998], 4))

# which words describe each topic? (this is the 'terms' side of the posterior)
print("the five most typical words of each topic:")
for topic in range(k):
    top = np.argsort(-terms_lda[topic])[:5]
    print("  topic %2d: %s" % (topic + 1, " ".join(lexicon_content[j] for j in top)))


# Additional challenge: choosing which k to use in LDA is a model selection problem. Typically,
# the best approach is to compute the LDA model like we have done above for each level of k, save
# the model log-likelihood and choose the k that produced the highest log-likelihood.


# # 4. Unsupervised Learning: word embeddings

# Word embeddings are an unsupervised learning technique that convert words into numerical
# vectors, enabling machine learning models to process texts. In this vector space, words with
# similar meanings (e.g., happy, joyful) are located close to each other, capturing semantic
# relationships between them.

# Our word embedding example has three steps. First, run word2vec to train a model using the
# training data split. Second, it uses the trained model to analyze the prediction data split.
# Third, it uses the constructed variables to forecast box office.

# Step 1 - Training Step

# ---- chunk 8 ----

# Obtain the column with the reviews and convert it to lower case
x = reviews_raw["full_text"].astype(str).str.lower().tolist()

# TODO: use a split of the data here (say 50%) instead of the entire dataset
split_at = round(0.5 * len(x))
x_lower = x[:split_at]

# number of topics in Word2Vec
total_topics_word2vec = 10

# The parameters of R's word2vec(x, type = "cbow", dim = 10, iter = 20). The remaining ones are
# that function's defaults: a window of five words, five negative examples per word, words that
# occur fewer than five times are ignored, and very frequent words are randomly skipped
WINDOW, NEGATIVE, MIN_COUNT, LEARNING_RATE, SAMPLE = 5, 5, 5, 0.05, 1e-3


# CBOW ("continuous bag of words") learns to predict a word from the words around it. Every word
# gets two vectors: one as a centre word (W_out) and one as context (W_in); the embedding we keep
# afterwards is the context one. For each word occurrence we take the average of the vectors of
# its neighbours and nudge the vectors so that this average scores high for the word that really
# stands there and low for five randomly drawn words that do not ("negative sampling").

# ---- chunk 9 ----

def word2vec_cbow(texts, dim, epochs, seed=1):

    rng = np.random.default_rng(seed)

    documents = [re.findall(r"[a-z0-9]+", text) for text in texts]

    counts = Counter(word for document in documents for word in document)
    vocabulary = [word for word, count in counts.items() if count >= MIN_COUNT]
    index = {word: i for i, word in enumerate(vocabulary)}
    n_vocab = len(vocabulary)
    frequency = np.array([counts[word] for word in vocabulary], dtype=float)

    corpus = [np.array([index[w] for w in d if w in index], dtype=np.int32) for d in documents]

    # keep-probability of the sub-sampling of frequent words (the formula word2vec uses)
    share = frequency / frequency.sum()
    keep_probability = np.clip((np.sqrt(share / SAMPLE) + 1) * (SAMPLE / share), 0, 1)

    # the distribution the negative examples are drawn from
    negative_probability = frequency ** 0.75
    negative_probability /= negative_probability.sum()

    W_in = (rng.random((n_vocab, dim)) - 0.5) / dim
    W_out = np.zeros((n_vocab, dim))

    started = time.time()

    for epoch in range(epochs):

        # the learning rate decreases as training proceeds
        lr = max(LEARNING_RATE * (1 - epoch / epochs), LEARNING_RATE * 1e-4)

        for document in corpus:

            if len(document) < 2:
                continue

            # randomly skip very frequent words
            document = document[rng.random(len(document)) < keep_probability[document]]
            length = len(document)

            for position in range(length):

                # a window of random width, as in the original word2vec
                width = rng.integers(1, WINDOW + 1)
                low, high = max(0, position - width), min(length, position + width + 1)
                context = np.concatenate((document[low:position], document[position + 1:high]))

                if len(context) == 0:
                    continue

                hidden = W_in[context].mean(axis=0)

                # the word that is really there, plus NEGATIVE words that are not
                targets = np.empty(NEGATIVE + 1, dtype=np.int64)
                targets[0] = document[position]
                targets[1:] = rng.choice(n_vocab, size=NEGATIVE, p=negative_probability)

                labels = np.zeros(NEGATIVE + 1)
                labels[0] = 1.0

                score = W_out[targets] @ hidden
                gradient = (labels - 1.0 / (1.0 + np.exp(-score))) * lr

                W_in[context] += (gradient @ W_out[targets]) / len(context)
                W_out[targets] += np.outer(gradient, hidden)

        print("  word2vec epoch %d of %d (%.0f s)" % (epoch + 1, epochs, time.time() - started))

    return {word: W_in[i] for i, word in enumerate(vocabulary)}


print("Training word2vec on %d reviews - this takes a couple of minutes" % len(x_lower))
embedding = word2vec_cbow(x_lower, dim=total_topics_word2vec, epochs=20, seed=1)
print("embedding: %d words x %d dimensions" % (len(embedding), total_topics_word2vec))


# Step 2 - Construct variables from word embeddings

# Similar to tutorial 1.1, we loop over all reviews.

# ---- chunk 10 ----

# TODO: Use the other split of the data here (say 50%) instead of the entire dataset
# R indexes this split as x[round(length(x)/2):length(x)], which starts one review earlier than
# the training split ends, so the two overlap in a single review. We keep the same rows here so
# that both versions work on the same data
x_upper = x[split_at - 1:]
total_reviews = len(x_upper)

# Create an empty matrix to store the posteriors
posteriors_w2v = np.zeros((total_reviews, total_topics_word2vec))

# Loop over all reviews
for k_review in range(total_reviews):

    # 2.1 get a review and tokenize it - identify the words, separately
    tokenized_review = re.findall(r"[a-z0-9]+", x_upper[k_review])

    # 2.2 get the word vectors per review. Words the model never saw have no vector; in R
    #     predict() returned NA for them and we averaged with na.rm = TRUE, so here we simply
    #     skip them
    embedding_review = [embedding[word] for word in tokenized_review if word in embedding]

    # 2.3 compute mean across all words for each column in the review
    if len(embedding_review) > 0:
        posteriors_w2v[k_review, :] = np.mean(embedding_review, axis=0)
    else:
        posteriors_w2v[k_review, :] = np.nan

print("posteriors_w2v shape:", posteriors_w2v.shape,
      " reviews without a single known word:", int(np.isnan(posteriors_w2v[:, 0]).sum()))


# Tip: for the above data splits, mind the time. Best to train in a split that temporarily
# precedes the prediction split

# Step 3 - Use the constructed variables to forecast

# ---- chunk 11 ----

# prepare the constructed variables for analysis. The box office figures come with thousands
# separators ("1,373,754"), so we strip the commas before taking logs
box_office = (reviews_raw["first_week_box_office"]
              .iloc[split_at - 1:]
              .astype(str).str.replace(",", "", regex=False))
log_BO = np.log(pd.to_numeric(box_office, errors="coerce").to_numpy(dtype=float))

data_reg = pd.DataFrame(posteriors_w2v,
                        columns=["w2v_%d" % (i + 1) for i in range(total_topics_word2vec)])
data_reg.insert(0, "LogBoxOffice", log_BO)

# forecast. R: lm(LogBoxOffice ~ posteriors_w2v, data = data_reg); statsmodels needs the
# intercept added explicitly, and we drop the rows with a missing value as lm() did
estimation_sample = data_reg.dropna()
y = estimation_sample["LogBoxOffice"]
X = sm.add_constant(estimation_sample.drop(columns="LogBoxOffice"))

w2v_BO_lm = sm.OLS(y, X).fit()
print(w2v_BO_lm.summary())

# the histogram of the outcome variable, as in the R version
all_box_office = (reviews_raw["first_week_box_office"]
                  .astype(str).str.replace(",", "", regex=False))
log_BO_all = np.log(pd.to_numeric(all_box_office, errors="coerce").to_numpy(dtype=float))

figure, axis = plt.subplots(figsize=(7, 4))
axis.hist(log_BO_all[~np.isnan(log_BO_all)], bins=30, color="#2C7FB8", edgecolor="white")
axis.set_xlabel("log(first week box office)")
axis.set_ylabel("count")
figure.tight_layout()
figure.savefig(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "tutorial1.2_2026_box_office_histogram.png"), dpi=150)

# show the plot on screen when we are running interactively (in Spyder, a notebook, ...)
if not matplotlib.get_backend().lower().startswith("agg"):
    plt.show()

# data_reg.to_csv("data_reg.csv", index=False)
