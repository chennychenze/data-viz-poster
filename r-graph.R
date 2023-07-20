# Chenze Chen
# IST 421 - Final Poster

library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(dplyr)
library(ggthemes)
library(treemapify)

dataset <-
  read.csv(
    "C:\\Users\\chenz\\OneDrive\\Desktop\\Fall 2022\\IST 421 - Information Visualization\\project\\leetcode_questions.csv"
  )

str(dataset)



# donut - 1
df.level <- as.data.frame(table(dataset$Difficulty.Level))
colnames(df.level) <- c("level", "count")

df.level$fraction = df.level$count / sum(df.level$count)

df.level$ymax = cumsum(df.level$fraction)
df.level$ymin = c(0, head(df.level$ymax, n = -1))

df.level$labelPosition <- (df.level$ymax + df.level$ymin) / 2
df.level$label <- paste0(df.level$level, ": ", df.level$count)

ggplot(df.level, aes(
  ymax = ymax,
  ymin = ymin,
  xmax = 2,
  xmin = 1,
  fill = level
)) +
  geom_rect() +
  geom_text(x = 1.5,
            aes(y = labelPosition, label = label),
            size = 3.5) +
  scale_fill_manual(values = c("green", "red", "orange")) +
  coord_polar(theta = "y") +
  xlim(c(0, 2)) +
  theme_void() +
  theme(legend.position = "none")



# word cloud - 2
topic <- tolower(unlist(strsplit(dataset$Topic.Tagged.text, "\\,")))
topic <- sort(table(topic), decreasing = TRUE)

df.topic <- as.data.frame(topic)
colnames(df.topic) <- c("word", "count")
wordcloud(
  words = df.topic$word,
  freq = df.topic$count,
  min.freq = 1,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(3, "Paired")
)



# box plot - 3
dataset$Difficulty.Level <-
  factor(dataset$Difficulty.Level, levels = c("Easy", "Medium", "Hard"))

ggplot(dataset) +
  aes(x = Difficulty.Level, y = Success.Rate, fill = Difficulty.Level) +
  geom_boxplot() +
  theme_tufte() +
  scale_fill_manual(values = c("red", "orange", "blue")) +
  theme(legend.position = "bottom")



# segment point - 4
df.like <- dataset[order(-dataset$Likes), ]
df.like <- df.like[c(1:10), c(2, 10)]

df.like %>%
  arrange(Likes) %>%
  mutate(name = factor(Question.Title, levels = Question.Title)) %>%
  ggplot(aes(x = name, y = Likes)) +
  geom_segment(aes(xend = name, yend = 0)) +
  geom_point(size = 4, color = "orange") +
  coord_flip() +
  theme_tufte() +
  geom_text(aes(label = Likes),
            position = position_dodge(width = 10),
            vjust = -1) +
  xlab("")



# main
df.array <- dataset %>% filter(grepl('Array', Topic.Tagged.text))
df.array <- as.data.frame(table(df.array$Difficulty.Level))
colnames(df.array) <- c("difficulty", "count")
df.array$tag <- c("array", "array", "array")


df.string <- dataset %>% filter(grepl('String', Topic.Tagged.text))
df.string <- as.data.frame(table(df.string$Difficulty.Level))
colnames(df.string) <- c("difficulty", "count")
df.string$tag <- c("string", "string", "string")


df.hash <-
  dataset %>% filter(grepl('Hash Table', Topic.Tagged.text))
df.hash <- as.data.frame(table(df.hash$Difficulty.Level))
colnames(df.hash) <- c("difficulty", "count")
df.hash$tag <- c("hash table", "hash table", "hash table")


df.dp <-
  dataset %>% filter(grepl('Dynamic Programming', Topic.Tagged.text))
df.dp <- as.data.frame(table(df.dp$Difficulty.Level))
colnames(df.dp) <- c("difficulty", "count")
df.dp$tag <-
  c("dynamic programming",
    "dynamic programming",
    "dynamic programming")


df.math <- dataset %>% filter(grepl('Math', Topic.Tagged.text))
df.math <- as.data.frame(table(df.math$Difficulty.Level))
colnames(df.math) <- c("difficulty", "count")
df.math$tag <- c("math", "math", "math")


df.dfs <-
  dataset %>% filter(grepl('Depth-First Search', Topic.Tagged.text))
df.dfs <- as.data.frame(table(df.dfs$Difficulty.Level))
colnames(df.dfs) <- c("difficulty", "count")
df.dfs$tag <-
  c("depth-first search",
    "depth-first search",
    "depth-first search")


df.sorting <-
  dataset %>% filter(grepl('Sorting', Topic.Tagged.text))
df.sorting <- as.data.frame(table(df.sorting$Difficulty.Level))
colnames(df.sorting) <- c("difficulty", "count")
df.sorting$tag <- c("sorting", "sorting", "sorting")


df.greedy <- dataset %>% filter(grepl('Greedy', Topic.Tagged.text))
df.greedy <- as.data.frame(table(df.greedy$Difficulty.Level))
colnames(df.greedy) <- c("difficulty", "count")
df.greedy$tag <- c("greedy", "greedy", "greedy")


df.db <- dataset %>% filter(grepl('Database', Topic.Tagged.text))
df.db <- as.data.frame(table(df.db$Difficulty.Level))
colnames(df.db) <- c("difficulty", "count")
df.db$tag <- c("database", "database", "database")


df.bfs <-
  dataset %>% filter(grepl('Breadth-First Search', Topic.Tagged.text))
df.bfs <- as.data.frame(table(df.bfs$Difficulty.Level))
colnames(df.bfs) <- c("difficulty", "count")
df.bfs$tag <-
  c("breadth-first search",
    "breadth-first search",
    "breadth-first search")


df.5 <-
  rbind(
    df.array,
    df.string,
    df.hash,
    df.dp,
    df.math,
    df.dfs,
    df.sorting,
    df.greedy,
    df.db,
    df.bfs
  )

ggplot(df.5) + aes(
  x = tag,
  y = count,
  fill = factor(difficulty, levels = c("Hard", "Medium", "Easy")),
  label = count
) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("#f6a0a1", "#f9d5a5", "#b1daae")) +
  theme_tufte() +
  theme(legend.position = "top") +
  scale_x_discrete(
    limits = c(
      "array",
      "string",
      "hash table",
      "dynamic programming",
      "math",
      "depth-first search",
      "sorting",
      "greedy",
      "database",
      "breadth-first search"
    )
  ) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))



# additional
df.array2 <- dataset %>% filter(grepl('Array', Topic.Tagged.text))
df.array2$tag <- c(rep("array", 1087))
df.arrayy <- aggregate(
  df.array2$total.submission,
  list(tag = df.array2$tag,
       reg = df.array2$Difficulty.Level),
  sum
)


df.string2 <- dataset %>% filter(grepl('String', Topic.Tagged.text))
df.string2$tag <- c(rep("string", 526))
df.stringg <- aggregate(
  df.string2$total.submission,
  list(tag = df.string2$tag,
       reg = df.string2$Difficulty.Level),
  sum
)


df.hash2 <-
  dataset %>% filter(grepl('Hash Table', Topic.Tagged.text))
df.hash2$tag <- c(rep("hash table", 372))
df.hashh <- aggregate(
  df.hash2$total.submission,
  list(tag = df.hash2$tag,
       reg = df.hash2$Difficulty.Level),
  sum
)


df.dp2 <-
  dataset %>% filter(grepl('Dynamic Programming', Topic.Tagged.text))
df.dp2$tag <- c(rep("dynamic programming", 362))
df.dpp <- aggregate(df.dp2$total.submission,
                    list(tag = df.dp2$tag,
                         reg = df.dp2$Difficulty.Level),
                    sum)


df.math2 <- dataset %>% filter(grepl('Math', Topic.Tagged.text))
df.math2$tag <- c(rep("math", 356))
df.mathh <- aggregate(
  df.math2$total.submission,
  list(tag = df.math2$tag,
       reg = df.math2$Difficulty.Level),
  sum
)


df.6 <- rbind(df.arrayy, df.stringg, df.dpp, df.hashh, df.mathh)
ggplot(df.6) + aes(area = x,
                   fill = reg,
                   subgroup = reg) +
  geom_treemap() +
  geom_treemap_subgroup_text(color = "white", place = "topleft") +
  geom_treemap_text(
    aes(label = tag),
    color = "black",
    place = "centre",
    grow = T
  ) +
  scale_fill_manual(values = c("#b1daae", "#f9d5a5", "#f6a0a1")) +
  geom_treemap_subgroup_border(colour = "white") +
  guides(fill = FALSE)