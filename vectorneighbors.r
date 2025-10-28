 #these neighbors reflect tyranny's semantic neighbors across the entire data set, all 27 years, using this as a way to historically validate the model
#note: the cosign similarities are fairly low, most at .10-.15, however this is probably due to the low word count coupled with the low frequency of tyranny in the data set, despite this the words can be historically contextualizd, proving the validity
#bar charts for the entire 27 years, then each period
model <- read.vectors("C:/Users/anton/Desktop/period_corpus/vectorsv2.bin")
neighbors <- nearest_to(model, model[["tyranny"]], 20)
x11()
df <- data.frame(
    word = names(neighbors),
    similarity = as.numeric(neighbors)
)

ggplot(df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1756-1783",
        x = "Neighbor",
        y = "Cosine Similarity"
    )
    ggsave("C:/Users/anton/Desktop/nhgazettevisualizations/semanticneighbors1756-1783.png")


model2 <- read.vectors("C:/Users/anton/Desktop/period_models/1756-1764_vectors.bin")

neighbors <- nearest_to(model2, model2[["tyranny"]], 20)
x11()
df <- data.frame(
    word = names(neighbors),
    similarity = as.numeric(neighbors)
)

ggplot(df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1756-1764",
        x = "Neighbor",
        y = "Cosine Similarity"
    )
ggsave("C:/Users/anton/Desktop/nhgazettevisualizations/semanticneighbors1756-1764.png")



model3 <- read.vectors("C:/Users/anton/Desktop/period_models/1765-1776_vectors.bin")


neighbors <- nearest_to(model3, model3[["tyranny"]], 20)
x11()
df <- data.frame(
    word = names(neighbors),
    similarity = as.numeric(neighbors)
)

ggplot(df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1765-1776",
        x = "Neighbor",
        y = "Cosine Similarity"
    )
ggsave("C:/Users/anton/Desktop/nhgazettevisualizations/semanticneighbors1765-1776.png")



model4 <- read.vectors("C:/Users/anton/Desktop/period_models/1777-1783_vectors.bin")


neighbors <- nearest_to(model4, model4[["tyranny"]], 20)
x11()
df <- data.frame(
    word = names(neighbors),
    similarity = as.numeric(neighbors)
)

ggplot(df, aes(x = reorder(word, similarity), y = similarity)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Semantic Neighbors of 'tyranny' 1777-1783",
        x = "Neighbor",
        y = "Cosine Similarity"
    )
ggsave("C:/Users/anton/Desktop/nhgazettevisualizations/semanticneighbors1777-1783.png")










# assumes: model <- read.vectors(".../vectors.bin")

seed <- "tyranny" # use the exact token in your model
n1 <- 20 # neighbors of seed
n2 <- 5 # neighbors of each neighbor

# 1) neighbors of seed
v_seed <- model[[seed]] # will error if OOV; if unsure, check: seed %in% rownames(model)
h1 <- nearest_to(model, v_seed, n = n1)
h1_tbl <- tibble(term = names(h1), sim = as.numeric(h1))

# 2) neighbors-of-neighbors
h2_tbl <- map_df(h1_tbl$term, function(t) {
    nn <- nearest_to(model, model[[t]], n = n2)
    tibble(from = t, term = names(nn), sim = as.numeric(nn))
})

# Results you can inspect/use:
h1_tbl # layer-1 list (neighbors of seed)
h2_tbl # layer-2 list (neighbors of each layer-1 term)

# Collect unique terms: seed + h1 + h2
terms <- unique(c(seed, h1_tbl$term, h2_tbl$term))
terms <- intersect(terms, rownames(model)) # drop OOV just in case

# Build cosine matrix in one shot (rows are normalized in wordVectors)
M <- as.matrix(model[terms, , drop = FALSE])
C <- tcrossprod(M) # cosine similarities
D <- 1 - C # convert to distance
hc <- hclust(as.dist(D), method = "ward.D2")

png("C:/Users/anton/Desktop/nhgazettevisualizations/semanticneighbors1756-1783dendro2.png", width = 1600, height = 1000, res = 200)

plot(hc, main = "Semantic Field of 'tyranny'", xlab = "", sub = "", cex = 0.8)
dev.off()
