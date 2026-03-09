library(ggplot2)
library(stats)
library(wordVectors)
library(dplyr)

# =========================================================
# 1. LOAD MODELS
# =========================================================

m1 <- read.vectors("period_models/1756-1764_vectors.bin")
m2 <- read.vectors("period_models/1765-1776_vectors.bin")
m3 <- read.vectors("period_models/1777-1783_vectors.bin")

shared_vocab <- Reduce(intersect, list(rownames(m1), rownames(m2), rownames(m3)))

# =========================================================
# 2. ALIGNMENT FUNCTION
# =========================================================

align_vectors <- function(base, target, vocab) {
    X <- base[vocab, ]
    Y <- target[vocab, ]

    Xc <- scale(X, center = TRUE, scale = FALSE)
    Yc <- scale(Y, center = TRUE, scale = FALSE)

    svd_result <- svd(t(Yc) %*% Xc)
    R <- svd_result$u %*% t(svd_result$v)

    Y_aligned <- Yc %*% R
    rownames(Y_aligned) <- vocab
    return(Y_aligned)
}

m2_aligned <- align_vectors(m1, m2, shared_vocab)
m3_aligned <- align_vectors(m1, m3, shared_vocab)

# =========================================================
# 3. COSINE FUNCTION
# =========================================================

cosine <- function(a, b) {
    sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

# =========================================================
# 4. TYRANNY ACROSS PERIODS
# =========================================================

tyr1 <- m1["tyranny", ]
tyr2 <- m2_aligned["tyranny", ]
tyr3 <- m3_aligned["tyranny", ]

similarities <- c(
    "1756-1764 → 1765-1776" = cosine(tyr1, tyr2),
    "1765-1776 → 1777-1783" = cosine(tyr2, tyr3),
    "1756-1764 → 1777-1783" = cosine(tyr1, tyr3)
)

similarities

# =========================================================
# 5. TYRANNY + PARLIAMENT WITHIN EACH PERIOD
# =========================================================

parl1 <- m1["parliament", ]
parl2 <- m2_aligned["parliament", ]
parl3 <- m3_aligned["parliament", ]

tyr_parliament <- c(
    "1756-1764" = cosine(tyr1, parl1),
    "1765-1776" = cosine(tyr2, parl2),
    "1777-1783" = cosine(tyr3, parl3)
)

tyr_parliament

df_tyrparliament <- data.frame(
    period = names(tyr_parliament),
    cosine = tyr_parliament
)

ggplot(df_tyrparliament, aes(x = period, y = cosine, group = 1)) +
    geom_line(linewidth = 1.2, color = "darkorange") +
    geom_point(size = 3, color = "firebrick") +
    ylim(0, 1) +
    labs(
        title = "Semantic Association Between 'Tyranny' and 'Parliament' Over Time",
        subtitle = "Cosine similarity within each period's aligned model",
        x = "Period",
        y = "Cosine Similarity (Higher = Stronger Association)"
    ) +
    theme_minimal(base_size = 14)

# =========================================================
# 6. SEMANTIC DRIFT OF TYRANNY ACROSS CONSECUTIVE PERIODS
# =========================================================

df_sim <- data.frame(
    period = c("1765–1776", "1777–1783"),
    similarity = c(similarities[1], similarities[2])
)

ggplot(df_sim, aes(x = period, y = similarity, group = 1)) +
    geom_line(linewidth = 1.2, color = "steelblue") +
    geom_point(size = 3, color = "red") +
    ylim(0, 1) +
    labs(
        title = "Semantic Drift of 'Tyranny' Across Periods",
        subtitle = "Cosine similarity between consecutive time-slices of aligned models",
        x = "Period",
        y = "Cosine Similarity (Higher = More Stable Meaning)"
    ) +
    theme_minimal(base_size = 14)

# =========================================================
# 7. TYRANNY + KING WITHIN EACH PERIOD
# =========================================================

king1 <- m1["king", ]
king2 <- m2_aligned["king", ]
king3 <- m3_aligned["king", ]

tyr_king <- c(
    "1756-1764" = cosine(tyr1, king1),
    "1765-1776" = cosine(tyr2, king2),
    "1777-1783" = cosine(tyr3, king3)
)

tyr_king

df_tyrking <- data.frame(
    period = names(tyr_king),
    cosine = tyr_king
)

# x11()
ggplot(df_tyrking, aes(x = period, y = cosine, group = 1)) +
    geom_hline(
        yintercept = 0,
        linetype = "dashed",
        linewidth = 0.6,
        color = "gray40"
    ) +
    geom_line(linewidth = 1.3, color = "#1b4f72") +
    geom_point(size = 3.5, color = "#922b21") +
    coord_cartesian(ylim = c(-0.15, 0.55)) +
    labs(
        title = "Semantic Association Between 'Tyranny' and 'King' (1756–1783)",
        subtitle = "Aligned embedding models; cosine similarity within each period",
        x = NULL,
        y = "Cosine Similarity"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray30"),
        axis.text.x = element_text(face = "bold"),
        panel.grid.minor = element_blank()
    )

ggsave("nhgazettevisualizations/tyrkingcosinesim.png", width = 10, height = 6, dpi = 300)




