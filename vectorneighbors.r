library(ggplot2)
library(stats)
library(wordVectors)
library(dplyr)
library(tibble)
library(purrr)
library(igraph)
library(ggraph)

dir.create("nhgazettevisualizations", recursive = TRUE, showWarnings = FALSE)

# =========================================================
# 1. FULL PERIOD MODEL: 1756-1783
# =========================================================

model_full <- read.vectors("period_corpus/vectorsv2.bin")

neighbors_full <- nearest_to(model_full, model_full[["tyranny"]], 20)

cosineSimilarity(model_full[["tyranny"]], model_full[["king"]])
cosineSimilarity(model_full[["tyranny"]], model_full[["parliament"]])
cosineSimilarity(model_full[["tyranny"]], model_full[["ministry"]])
cosineSimilarity(model_full[["tyranny"]], model_full[["crown"]])

neighbors_full_df <- data.frame(
    word = names(neighbors_full),
    similarity = 1 - as.numeric(neighbors_full)
)

# x11()
ggplot(neighbors_full_df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1756-1783",
        x = "Neighbor",
        y = "Cosine Similarity"
    )

ggsave("nhgazettevisualizations/semanticneighbors1756-1783.png")

# =========================================================
# 2. EARLY PERIOD MODEL: 1756-1764
# =========================================================

model_1756_1764 <- read.vectors("period_models/1756-1764_vectors.bin")

cosineSimilarity(model_1756_1764[["tyranny"]], model_1756_1764[["france"]])
cosineSimilarity(model_1756_1764[["tyranny"]], model_1756_1764[["french"]])
cosineSimilarity(model_1756_1764[["tyranny"]], model_1756_1764[["catholic"]])
cosineSimilarity(model_1756_1764[["tyranny"]], model_1756_1764[["popery"]])

cosineSimilarity(model_1756_1764[["tyranny"]], model_1756_1764[["king"]])
cosineSimilarity(model_1756_1764[["tyranny"]], model_1756_1764[["parliament"]])
cosineSimilarity(model_1756_1764[["tyranny"]], model_1756_1764[["ministry"]])
cosineSimilarity(model_1756_1764[["tyranny"]], model_1756_1764[["crown"]])

# PCA of selected terms
early_terms <- c("tyranny", "france", "french", "catholic", "popery")
early_mat <- as.matrix(model_1756_1764[early_terms, ])
early_pca <- prcomp(early_mat, scale. = FALSE)
early_scores <- as.data.frame(early_pca$x)
early_scores$term <- rownames(early_mat)
var_explained <- early_pca$sdev^2 / sum(early_pca$sdev^2)
x11()
ggplot(early_scores, aes(PC1, PC2, label = term)) +
    geom_point(size = 3) +
    geom_text(vjust = -0.6, size = 4) +
    labs(
        x = paste0("Principle Component 1 (", round(var_explained[1] * 100, 1), "%)"),
        y = paste0("Principle Component 2 (", round(var_explained[2] * 100, 1), "%)")
    ) +
    theme_minimal()

ggsave("nhgazettevisualizations/tyrannypca1756-1764.png")

neighbors_1756_1764 <- nearest_to(model_1756_1764, model_1756_1764[["tyranny"]], 20)

neighbors_1756_1764_df <- data.frame(
    word = names(neighbors_1756_1764),
    similarity = 1 - as.numeric(neighbors_1756_1764)
)

# x11()
ggplot(neighbors_1756_1764_df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1756-1764",
        x = "Neighbor",
        y = "Cosine Similarity"
    )

ggsave("nhgazettevisualizations/semanticneighbors1756-1764.png")

early_neighbor_terms <- c("tyranny", names(neighbors_1756_1764))
early_neighbor_mat <- as.matrix(model_1756_1764[early_neighbor_terms, ])
early_neighbor_pca <- prcomp(early_neighbor_mat, scale. = FALSE)
early_neighbor_scores <- as.data.frame(early_neighbor_pca$x)
early_neighbor_scores$term <- rownames(early_neighbor_mat)

# x11()
ggplot(early_neighbor_scores, aes(x = PC1, y = PC2, label = term)) +
    geom_point(size = 3) +
    geom_text(vjust = -0.6) +
    theme_minimal(base_size = 14) +
    labs(
        title = "PCA of Tyranny and Its Nearest Neighbors, 1756-1764",
        x = "PC1",
        y = "PC2"
    )

ggsave("nhgazettevisualizations/tyrannypca1756-1764neighbors.png")

# =========================================================
# 3. MIDDLE PERIOD MODEL: 1765-1776
# =========================================================

model_1765_1776 <- read.vectors("period_models/1765-1776_vectors.bin")

cosineSimilarity(model_1765_1776[["tyranny"]], model_1765_1776[["king"]])
cosineSimilarity(model_1765_1776[["tyranny"]], model_1765_1776[["parliament"]])
cosineSimilarity(model_1765_1776[["tyranny"]], model_1765_1776[["ministry"]])
cosineSimilarity(model_1765_1776[["tyranny"]], model_1765_1776[["crown"]])

neighbors_1765_1776 <- nearest_to(model_1765_1776, model_1765_1776[["tyranny"]], 20)

neighbors_1765_1776_df <- data.frame(
    word = names(neighbors_1765_1776),
    similarity = 1 - as.numeric(neighbors_1765_1776)
)

# x11()
ggplot(neighbors_1765_1776_df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1765-1776",
        x = "Neighbor",
        y = "Cosine Similarity"
    )

ggsave("nhgazettevisualizations/semanticneighbors1765-1776.png")

middle_neighbor_terms <- c("tyranny", names(neighbors_1765_1776))
middle_neighbor_mat <- as.matrix(model_1765_1776[middle_neighbor_terms, ])
middle_neighbor_pca <- prcomp(middle_neighbor_mat, scale. = FALSE)
middle_neighbor_scores <- as.data.frame(middle_neighbor_pca$x)
middle_neighbor_scores$term <- rownames(middle_neighbor_mat)

# x11()
ggplot(middle_neighbor_scores, aes(x = PC1, y = PC2, label = term)) +
    geom_point(size = 3) +
    geom_text(vjust = -0.6) +
    theme_minimal(base_size = 14) +
    labs(
        title = "PCA of Tyranny and Its Nearest Neighbors, 1765-1776",
        x = "PC1",
        y = "PC2"
    )

ggsave("nhgazettevisualizations/tyrannypca1765-1776neighbors.png")

# =========================================================
# 4. LATE PERIOD MODEL: 1777-1783
# =========================================================

model_1777_1783 <- read.vectors("period_models/1777-1783_vectors.bin")

neighbors_1777_1783 <- nearest_to(model_1777_1783, model_1777_1783[["tyranny"]], 20)
britain_neighbors <- nearest_to(model_1777_1783, model_1777_1783[["britain"]], 20)

neighbors_1777_1783_df <- data.frame(
    word = names(neighbors_1777_1783),
    similarity = 1 - as.numeric(neighbors_1777_1783)
)

# x11()
ggplot(neighbors_1777_1783_df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1777-1783",
        x = "Neighbor",
        y = "Cosine Similarity"
    )

ggsave("nhgazettevisualizations/semanticneighbors1777-1783.png")

cosineSimilarity(model_1777_1783[["tyranny"]], model_1777_1783[["king"]])
cosineSimilarity(model_1777_1783[["tyranny"]], model_1777_1783[["parliament"]])
cosineSimilarity(model_1777_1783[["tyranny"]], model_1777_1783[["ministry"]])
cosineSimilarity(model_1777_1783[["tyranny"]], model_1777_1783[["crown"]])

late_neighbor_terms <- c("tyranny", names(neighbors_1777_1783))
late_neighbor_mat <- as.matrix(model_1777_1783[late_neighbor_terms, ])
late_neighbor_pca <- prcomp(late_neighbor_mat, scale. = FALSE)
late_neighbor_scores <- as.data.frame(late_neighbor_pca$x)
late_neighbor_scores$term <- rownames(late_neighbor_mat)

# x11()
ggplot(late_neighbor_scores, aes(x = PC1, y = PC2, label = term)) +
    geom_point(size = 3) +
    geom_text(vjust = -0.6) +
    theme_minimal(base_size = 14) +
    labs(
        title = "PCA of Tyranny and Its Nearest Neighbors, 1777-1783",
        x = "PC1",
        y = "PC2"
    )

ggsave("nhgazettevisualizations/tyrannypca1777-1783neighbors.png")

# =========================================================
# 5. DENDROGRAM OF NEIGHBORS-OF-NEIGHBORS
# =========================================================

seed <- "tyranny"
n1 <- 20
n2 <- 3

v_seed <- model_full[[seed]]
h1 <- nearest_to(model_full, v_seed, n = n1)
h1_tbl <- tibble(term = names(h1), sim = as.numeric(h1))

h2_tbl <- map_df(h1_tbl$term, function(t) {
    nn <- nearest_to(model_full, model_full[[t]], n = n2)
    tibble(from = t, term = names(nn), sim = as.numeric(nn))
})

term_list <- unique(c(seed, h1_tbl$term, h2_tbl$term))
term_list <- base::intersect(term_list, rownames(model_full))

M <- as.matrix(model_full[term_list, , drop = FALSE])
C <- tcrossprod(M)
D <- 1 - C
hc <- hclust(as.dist(D), method = "ward.D2")

png("nhgazettevisualizations/semanticneighbors1756-1783dendro2.png", width = 1600, height = 1000, res = 200)
plot(
    hc,
    main = "Semantic Field of 'tyranny'",
    xlab = "",
    sub = ""
)
dev.off()

# =========================================================
# 6. NETWORK GRAPH OF NEIGHBORS-OF-NEIGHBORS
# =========================================================

network_h2_tbl <- map_df(h1_tbl$term, function(t) {
    nn <- nearest_to(model_full, model_full[[t]], n = n2)
    tibble(
        from = t,
        term = names(nn),
        sim = as.numeric(nn)
    )
}) %>%
    filter(from != term) %>%
    mutate(weight = 1 - sim)

edges <- network_h2_tbl %>%
    select(from, term, sim, weight)

g <- graph_from_data_frame(edges, directed = FALSE)

E(g)$edge_id <- seq_len(ecount(g))

set.seed(123)

# x11()
ggraph(g, layout = "fr") +
    geom_edge_link(
        aes(alpha = weight, width = weight),
        colour = "gray50",
        show.legend = FALSE
    ) +
    geom_node_point(
        aes(color = as.factor(membership(cluster_louvain(g)))),
        size = 5
    ) +
    geom_node_text(
        aes(label = name),
        repel = TRUE,
        size = 3.5,
        max.iter = 2000
    ) +
    scale_edge_alpha(range = c(0.1, 1)) +
    scale_edge_width(range = c(0.3, 2.5)) +
    scale_color_brewer(palette = "Set2") +
    theme_void() +
    labs(
        title = paste0("Semantic neighbourhood of '", seed, "'"),
        subtitle = paste0(n1, " first-order / ", n2, " second-order neighbours")
    )

ggsave("nhgazettevisualizations/semanticneighbors1756-1783network.png")





