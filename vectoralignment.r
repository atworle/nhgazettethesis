
library(ggplot2)
library(stats)
library(wordVectors)
library(dplyr)

m1 <- read.vectors("period_models/1756-1764_vectors.bin")
m2 <- read.vectors("period_models/1765-1776_vectors.bin")
m3 <- read.vectors("period_models/1777-1783_vectors.bin")

shared_vocab <- Reduce(intersect, list(rownames(m1), rownames(m2), rownames(m3)))

align_vectors <- function(base, target, vocab) {
    X <- base[vocab, ]
    Y <- target[vocab, ]

    # center (important for stability)
    Xc <- scale(X, center = TRUE, scale = FALSE)
    Yc <- scale(Y, center = TRUE, scale = FALSE)

    # SVD-based rotation
    svd_result <- svd(t(Yc) %*% Xc)
    R <- svd_result$u %*% t(svd_result$v)

    # Rotate target
    Y_aligned <- Yc %*% R
    rownames(Y_aligned) <- vocab
    return(Y_aligned)
}

m2_aligned <- align_vectors(m1, m2, shared_vocab)
m3_aligned <- align_vectors(m1, m3, shared_vocab)

cosine <- function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

tyr1 <- m1["tyranny", ]
tyr2 <- m2_aligned["tyranny", ]
tyr3 <- m3_aligned["tyranny", ]

similarities <- c(
    "1756-1764 → 1765-1776" = cosine(tyr1, tyr2),
    "1765-1776 → 1777-1783" = cosine(tyr2, tyr3),
    "1756-1764 → 1777-1783" = cosine(tyr1, tyr3)
)

similarities

pop1 <- m1["popery", ]
pop2 <- m2_aligned["popery", ]
pop3 <- m3_aligned["popery", ]

tyr_pop <- c(
    "1756-1764" = cosine(tyr1, pop1),
    "1765-1776" = cosine(tyr2, pop2),
    "1777-1783" = cosine(tyr3, pop3)
)

tyr_pop


nearest_from_matrix <- function(M, word, n = 10) {
    if (!word %in% rownames(M)) stop(sprintf("'%s' not in rownames(M)", word))
    v <- M[word, , drop = TRUE]
    sims <- (M %*% v) / (sqrt(rowSums(M^2)) * sqrt(sum(v^2)))
    sims <- sort(as.vector(sims), decreasing = TRUE)
    sims <- sims[names(sims) != word] # drop the word itself
    head(sims, n)
}
#need to continue developing this code
