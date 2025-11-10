library(ggplot2)
library(stats)
library(wordVectors)
library(dplyr)
library(tibble)
library(purrr)
library(igraph)
library(ggraph)
library(tidyverse)
 #these neighbors reflect tyranny's semantic neighbors across the entire data set, all 27 years, using this as a way to historically validate the model
#note: the cosign similarities are fairly low, most at .10-.15, however this is probably due to the low word count coupled with the low frequency of tyranny in the data set, despite this the words can be historically contextualizd, proving the validity
#bar charts for the entire 27 years, then each period
model <- read.vectors("period_corpus/vectorsv2.bin")
neighbors <- nearest_to(model, model[["tyranny"]], 20)
x11()
df <- data.frame(
    word = names(neighbors),
    similarity =1- as.numeric(neighbors)
)

ggplot(df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1756-1783",
        x = "Neighbor",
        y = "Cosine Similarity"
    )
    
dir.create("nhgazettevisualizations", recursive = TRUE, showWarnings = FALSE)
ggsave("nhgazettevisualizations/semanticneighbors1756-1783.png")


model2 <- read.vectors("period_models/1756-1764_vectors.bin")

neighbors2 <- nearest_to(model2, model2[["tyranny"]], 20)
x11()
df <- data.frame(
    word = names(neighbors2),
    similarity = 1- as.numeric(neighbors2)
)

ggplot(df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1756-1764",
        x = "Neighbor",
        y = "Cosine Similarity"
    )
ggsave("nhgazettevisualizations/semanticneighbors1756-1764.png")


model3 <- read.vectors("period_models/1765-1776_vectors.bin")


neighbors3 <- nearest_to(model3, model3[["tyranny"]], 20)
x11()
df <- data.frame(
    word = names(neighbors3),
    similarity = 1- as.numeric(neighbors3)
)

ggplot(df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1765-1776",
        x = "Neighbor",
        y = "Cosine Similarity"
    )
ggsave("nhgazettevisualizations/semanticneighbors1765-1776.png")




model4 <- read.vectors("period_models/1777-1783_vectors.bin")


neighbors4 <- nearest_to(model4, model4[["tyranny"]], 20)
x11()
df <- data.frame(
    word = names(neighbors4),
    similarity = 1- as.numeric(neighbors4)
)

ggplot(df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1777-1783",
        x = "Neighbor",
        y = "Cosine Similarity"
    )
ggsave("nhgazettevisualizations/semanticneighbors1777-1783.png")




#this is building dendro of neighbors of neighbors
seed <- "tyranny"
n1 <- 20
n2 <- 3

# 1) neighbors of seed
v_seed <- model[[seed]]
#h1 is neighbors of tyranny
h1 <- nearest_to(model, v_seed, n = n1)
h1_tbl <- tibble(term = names(h1), sim = as.numeric(h1))

#h2 is neighbors of neighbors
h2_tbl <- map_df(h1_tbl$term, function(t) {
    nn <- nearest_to(model, model[[t]], n = n2)
    tibble(from = t, term = names(nn), sim = as.numeric(nn))
})

# renamed to avoid conflict with the base R function 'terms'
term_list <- unique(c(seed, h1_tbl$term, h2_tbl$term))
term_list <- base::intersect(term_list, rownames(model)) # ensure all are in vocab


M <- as.matrix(model[term_list, , drop = FALSE])
dim(M)

C <- tcrossprod(M)
D <- 1 - C
hc <- hclust(as.dist(D), method = "ward.D2")

#dendro plot
plot(hc,
    main = "Semantic Field of 'tyranny'",
    xlab = "", sub = ""
)
png("nhgazettevisualizations/semanticneighbors1756-1783dendro2.png", width = 1600, height = 1000, res = 200)

dev.off()


#this is building network node graph of neighbors of neighbors

edges <- h2_tbl %>%
    select(from, term, sim) %>%
    filter(from != term) %>%
    mutate(weight = 1 - sim)



# Build graph



g <- igraph::simplify(g, edge.attr.comb = list(weight = "max")) 
comm <- cluster_louvain(g)

# Plot with ggraph
x11()
ggraph(g, layout = "fr") +
    geom_edge_link(aes(alpha = weight), color = "gray60") +
    geom_node_point(aes(color = as.factor(membership(comm))), size = 4) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_edge_alpha(range = c(0.2, 1)) +
    theme_void() +
    labs(title = "Semantic Neighborhood of 'tyranny'")

ggsave("nhgazettevisualizations/semanticneighbors1756-1783network.png")



#used grok and chatgpt to refine the above network plot
seed <- "tyranny"
n1 <- 20 # 1st-order neighbours
n2 <- 3 # 2nd-order neighbours

# ---- 1st order -------------------------------------------------
v_seed <- model[[seed]]
h1 <- nearest_to(model, v_seed, n = n1)
h1_tbl <- tibble(term = names(h1), sim = as.numeric(h1))

# ---- 2nd order -------------------------------------------------
h2_tbl <- map_df(h1_tbl$term, function(t) {
    nn <- nearest_to(model, model[[t]], n = n2)
    tibble(
        from = t,
        term = names(nn),
        sim  = as.numeric(nn)
    )
}) %>%
    filter(from != term) %>% # drop self-loops
    mutate(weight = 1 - sim) # edge strength for alpha/width

edges <- h2_tbl %>% select(from, term, sim, weight)

g <- graph_from_data_frame(edges, directed = FALSE)

# OPTIONAL: give every parallel edge a unique ID (helps ggraph)
E(g)$edge_id <- seq_len(ecount(g))

set.seed(123) # reproducible layout
x11()
ggraph(g, layout = "fr") +
    # ---- draw every edge, thickness/alpha = cosine strength ----
    geom_edge_link(aes(alpha = weight, width = weight),
        colour = "gray50", show.legend = FALSE
    ) +

    # ---- nodes ----------------------------------------------------
    geom_node_point(aes(color = as.factor(membership(cluster_louvain(g)))),
        size = 5
    ) +

    geom_node_text(aes(label = name),
        repel = TRUE, size = 3.5,
        max.iter = 2000
    ) +

    # ---- scales ----------------------------------------------------
    scale_edge_alpha(range = c(0.1, 1)) +
    scale_edge_width(range = c(0.3, 2.5)) +

    scale_color_brewer(palette = "Set2") +

    theme_void() +
    labs(
        title = paste0("Semantic neighbourhood of '", seed, "'"),
        subtitle = paste0(n1, " first-order / ", n2, " second-order neighbours")
    )