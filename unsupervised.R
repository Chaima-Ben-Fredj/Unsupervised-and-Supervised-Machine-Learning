library(tidyverse)
library(ggplot2)
library(GGally) 
library(reshape2)  
library(pheatmap)
library(dplyr)
library(caret)
library(cluster)
library(factoextra)
library(patchwork)
library(maps)
library(sf)
library(rnaturalearth)
library(knitr)
library(kableExtra)
library(Rtsne)
library(ggpubr)



#----------------------------EDA-----------------------------

df <- read.csv("C:/Users/PCS/Desktop/DSE/second trimester/stat learning/Unsupervised learning/Country-data.csv")
head(df)
str(df)
summary(df)
colSums(is.na(df))
#no na values


#------Univariate Analysis
#distribution of numerical variables
df %>%
  select(-country) %>%  
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~key, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Numerical Variables")

#Detecting Outliers
df %>%
  select(-country) %>%
  gather() %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "steelblue") +
  facet_wrap(~key, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots of Numerical Variables")

#----------Bivariate Analysis
#scatterplots:
my_theme <- theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"))

# 1. Child Mortality vs GDP per Capita 
p1 <- ggplot(df, aes(x = gdpp, y = child_mort)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 0.8) +
  geom_smooth(method = "loess", color = "grey", se = FALSE) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Child Mortality vs GDP per Capita",
       x = "GDP per capita (log scale, USD)",
       y = "Child Mortality (per 1000 live births)") +
  my_theme
print(p1)


# 2. Life Expectancy vs Health Spending
p2 <- ggplot(df, aes(x = health, y = life_expec)) +
  geom_point(alpha = 0.6, color = "darkgreen", size = 0.8) +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Life Expectancy vs Health Spending",
       x = "Health Spending (% of GDP)",
       y = "Life Expectancy (years)") +
  my_theme
print(p2)


# 3. Total Fertility vs Income
p3 <- ggplot(df, aes(x = income, y = total_fer)) +
  geom_point(alpha = 0.6, color = "purple", size = 0.8) +
  geom_smooth(method = "loess", color = "grey", se = FALSE) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Fertility Rate vs Income",
       x = "Net Income per person (log scale, USD)",
       y = "Total Fertility Rate") +
  my_theme
print(p3)


# 4. Exports vs Imports 
p4 <- ggplot(df, aes(x = exports, y = imports)) +
  geom_point(alpha = 0.6, color = "darkorange", size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  labs(title = "Exports vs Imports",
       x = "Exports (% of GDP per capita)",
       y = "Imports (% of GDP per capita)") +
  my_theme
print(p4)


# 5. Inflation vs GDP Growth 
p5 <- ggplot(df, aes(x = gdpp, y = inflation)) +
  geom_point(alpha = 0.6, color = "brown", size = 0.8) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Inflation vs GDP per Capita",
       x = "GDP per capita (log scale, USD)",
       y = "Inflation Rate (%)") +
  my_theme
print(p5)


# 6. Life Expectancy vs Child Mortality 
p6 <- ggplot(df, aes(x = life_expec, y = child_mort)) +
  geom_point(alpha = 0.6, color = "darkred", size = 0.8) +
  labs(title = "Life Expectancy vs Child Mortality",
       x = "Life Expectancy (years)",
       y = "Child Mortality (per 1000 live births)") +
  my_theme
print(p6)


#--countries wrt gdp per capita (for each plot we select 5 countries(top5 high, top5 medium, top5 low )after sorting the data by GDP)

colors <- c("deepskyblue4","powderblue", "deepskyblue4","powderblue","deepskyblue4")
#colors <- c("deepskyblue4","#1f77b4", "turquoise","skyblue","powderblue")#,"rosybrown1" ,"thistle1")

data_sorted <- df %>% arrange(desc(gdpp))

top5 <- data_sorted %>% slice(1:5)
p1 <- ggplot(top5, aes(x = reorder(country, -gdpp), y = gdpp)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with High GDP per Capita",
       x = "", y = "GDP per capita") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

middle5 <- data_sorted %>% slice(81:85)
p2 <- ggplot(middle5, aes(x = reorder(country, -gdpp), y = gdpp)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with Medium GDP per Capita",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bottom5 <- data_sorted %>% slice(161:165)
p3 <- ggplot(bottom5, aes(x = reorder(country, -gdpp), y = gdpp)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with Low GDP per Capita",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

(p1 | p2 | p3) + 
  plot_layout(widths = c(1, 1, 1)) +
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 1)))

#countries wrt income 
data_sorted <- df %>% arrange(desc(income))

top5 <- data_sorted %>% slice(1:5)
p1 <- ggplot(top5, aes(x = reorder(country, -income), y = income)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with High Net Income per Person",
       x = "", y = "Net Income per Person") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

middle5 <- data_sorted %>% slice(81:85)
p2 <- ggplot(middle5, aes(x = reorder(country, -income), y = income)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with Medium Net Income per Person",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bottom5 <- data_sorted %>% slice(161:165)
p3 <- ggplot(bottom5, aes(x = reorder(country, -income), y = income)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with Low Net Income per Person",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

(p1 | p2 | p3) + 
  plot_layout(widths = c(1, 1, 1)) +
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 1)))

#countries wrt inflation 
data_sorted <- df %>% arrange(desc(inflation))

top5 <- data_sorted %>% slice(1:5)
p1 <- ggplot(top5, aes(x = reorder(country, -inflation), y = inflation)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with High Inflation",
       x = "", y = "Inflation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

middle5 <- data_sorted %>% slice(81:85)
p2 <- ggplot(middle5, aes(x = reorder(country, -inflation), y = inflation)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with Medium Inflation",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bottom5 <- data_sorted %>% slice(161:165)
p3 <- ggplot(bottom5, aes(x = reorder(country, -inflation), y = inflation)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with Low Inflation",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

(p1 | p2 | p3) + 
  plot_layout(widths = c(1, 1, 1)) +
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 1)))

#countries wrt child mortality 
data_sorted <- df %>% arrange(desc(child_mort))

top5 <- data_sorted %>% slice(1:5)
p1 <- ggplot(top5, aes(x = reorder(country, -child_mort), y = child_mort)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with High Child Mortality",
       x = "", y = "Child Mortality") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

middle5 <- data_sorted %>% slice(81:85)
p2 <- ggplot(middle5, aes(x = reorder(country, -child_mort), y = child_mort)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with Medium Child Mortality",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bottom5 <- data_sorted %>% slice(161:165)
p3 <- ggplot(bottom5, aes(x = reorder(country, -child_mort), y = child_mort)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with Low Child Mortality",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

(p1 | p2 | p3) + 
  plot_layout(widths = c(1, 1, 1)) +
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 1)))


#countries wrt Health
data_sorted <- df %>% arrange(desc(health))

top5 <- data_sorted %>% slice(1:5)
p1 <- ggplot(top5, aes(x = reorder(country, -health), y = health)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with High Health Spending",
       x = "", y = "Health Spending") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

middle5 <- data_sorted %>% slice(81:85)
p2 <- ggplot(middle5, aes(x = reorder(country, -health), y = health)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with Medium Health Spending",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bottom5 <- data_sorted %>% slice(161:165)
p3 <- ggplot(bottom5, aes(x = reorder(country, -health), y = health)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(title = "Countries with Low Health Spending",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

(p1 | p2 | p3) + 
  plot_layout(widths = c(1, 1, 1)) +
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 1)))

#displaying the map for each variable

# Finding countries in the dtataset that do not match world map
anti_join(df, map_data("world"), by = c("country" = "region")) %>%
  distinct(country)
name_conversions <- c(
  "Antigua and Barbuda" = "Antigua",
  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
  "Congo, Rep." = "Republic of Congo",
  "Cote d'Ivoire" = "Ivory Coast",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Lao" = "Laos",
  "Macedonia, FYR" = "North Macedonia",
  "Micronesia, Fed. Sts." = "Micronesia",
  "Slovak Republic" = "Slovakia",
  "St. Vincent and the Grenadines" = "Saint Vincent",
  "United Kingdom" = "UK",
  "United States" = "USA"
)


df_map <- df %>%
  mutate(
    map_name = ifelse(country %in% names(name_conversions),
                      name_conversions[country],
                      country)
  )
anti_join(df_map, map_data("world"), by = c("map_name" = "region")) %>%
  distinct(country, map_name)

world_map <- map_data("world") %>%
  left_join(df_map, by = c("region" = "map_name"))

numeric_vars <- df_map %>% select(where(is.numeric)) %>% names()

# let's visualize the maps
for (var in numeric_vars) {
  p <- ggplot(world_map, aes(long, lat, group = group, fill = .data[[var]])) +
    geom_polygon(color = "lightgrey", linewidth = 0.1) +
    scale_fill_gradient(low = "slategray1", high = "royalblue4", name = var) +
    labs(title = paste(var, "Per Country")) +
    theme_void()
  
  ggsave(paste0("choropleth_", var, ".png"), p, width = 10, height = 6)
}


#Multivariate Analysis
#correlation matrix
cor_matrix <- df %>% 
  select(-country) %>%  
  cor(use = "complete.obs") %>%  
  round(2)  
corrplot::corrplot(cor_matrix, method = "color", type = "lower", 
                   tl.col = "black", tl.srt = 45, 
                   addCoef.col = "black", number.cex = 0.7)
corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                   tl.col = "black", tl.srt = 45, 
                   addCoef.col = "black", number.cex = 0.7)


# Heatmap
pheatmap(
  cor_matrix,
  color = colorRampPalette(c("darkgrey", "white", "dodgerblue4"))(100),  
  border_color = "gray",                                     
  cluster_rows = TRUE,   
  cluster_cols = TRUE,   
  clustering_method = "complete",  
  clustering_distance_rows = "euclidean",  
  clustering_distance_cols = "euclidean",
  display_numbers = TRUE,          
  number_color = "black",          
  fontsize_number = 8,             
  main = "Correlation Heatmap",  
  angle_col = 45,                  
  treeheight_row = 0,             
  treeheight_col = 0
)

#--------------------------------Models-------------------------------------------

#------data scaling
#we will use Standardization (Z-Score) scaling since it preserves the relative differences 
#between countries while minimizing outlier impact:

df_numeric <- df %>% 
  select(where(is.numeric), -income)  #from the EDA part we found that income and gdp are highly correlated so we're removing income

df_scaled <- scale(df_numeric) %>% 
  as.data.frame()

head(df_scaled)
summary(df_scaled)

#------PCA
pca_result <- prcomp(df_scaled, center = TRUE, scale. = FALSE)  
summary(pca_result)
#scree plot for explained variance
fviz_eig(pca_result, 
         addlabels = TRUE, 
         barfill = "steelblue",
         barcolor = "steelblue",
         linecolor = "salmon",
         ylim = c(0, 50)) +  
  labs(title = "Scree Plot: PCA Explained Variance",
       x = "Principal Components",
       y = "Percentage of Explained Variance") +
  theme_minimal()


# Explained variance and cumulative explained variance
var_data <- data.frame(
  PC = 1:8,
  Individual = pca_result$sdev^2 / sum(pca_result$sdev^2),
  Cumulative = cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
)

ggplot(var_data, aes(x = factor(PC))) +
  geom_col(aes(y = Individual, fill = "Individual Variance"), 
           alpha = 0.7, width = 0.6) +
  geom_line(aes(y = Cumulative, color = "Cumulative Variance", group = 1), 
            linewidth = 1) + 
  geom_point(aes(y = Cumulative, color = "Cumulative Variance"), 
             size = 3) +
  labs(
    x = "Principal Components", 
    y = "Variance Explained",
    fill = "",
    color = ""
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Individual Variance" = "steelblue")) +
  scale_color_manual(values = c("Cumulative Variance" = "salmon")) +
  theme_minimal() +
  theme(
    legend.position = "top",  
    legend.box = "horizontal" 
  )



# we will Keep PC1-PC3 since we have cumulative variance = 77.8%
df_pca <- as.data.frame(pca_result$x[, 1:3])

loadings <- pca_result$rotation[, 1:3]
round(loadings, 2)
fviz_pca_var(pca_result, axes = c(1, 2), col.var = "contrib", repel = TRUE)
fviz_pca_var(pca_result, axes = c(1, 3), col.var = "contrib", repel = TRUE)
fviz_pca_var(pca_result, axes = c(2, 3), col.var = "contrib", repel = TRUE)

loadings_all <- pca_result$rotation
loadings_top3 <- as.data.frame(loadings_all[, 1:3])
loadings_rounded <- round(loadings_top3, 3)
print(loadings_rounded)

kable(loadings_rounded, 
      caption = "PCA Loadings (Top 3 Components)",
      align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover"))



#----------k-means clustering
#choosing k:
#elbow method:
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(title = "Elbow Method: Optimal k")

#silhouette method
fviz_nbclust(df_scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette Score: Optimal k")


set.seed(123)  
k <- 3  
kmeans_result <- kmeans(df_scaled, centers = k, nstart = 25)
df$cluster <- as.factor(kmeans_result$cluster)


#visualizing our clusters
fviz_cluster(
  kmeans_result,
  data = df_scaled,
  palette = RColorBrewer::brewer.pal(3, "Set2"),
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "top"
    )
) + 
  labs(title = "Country Clusters (K-means)")


#since we don't know which cluster corresponds to which group of countries we will evaluate each cluster based on child
#mortaility and gdpp since high child mortality rates and low gdpp are associated with least developed countries

# GDP comparison
p1 <- ggplot(df, aes(as.factor(kmeans_result$cluster), gdpp, fill = as.factor(kmeans_result$cluster))) +
  geom_boxplot() +
  scale_y_log10(labels = scales::dollar) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Cluster", y = "GDP per capita") +
  theme(legend.position = "none")

# Child mortality comparison
p2 <- ggplot(df, aes(as.factor(kmeans_result$cluster), child_mort, fill = as.factor(kmeans_result$cluster))) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Cluster", y = "Child Mortality") +
  theme(legend.position = "none")

ggarrange(p1, p2, ncol = 2)

#cluster summary table with just child mortality and gdpp 
df %>%
  mutate(cluster = kmeans_result$cluster) %>%
  group_by(cluster) %>%
  summarise(
    Countries = n(), 
    across(c(child_mort, gdpp),
           ~ round(mean(.), 2))
  )


# we will create a summary table with counts and means for all the variables
cluster_summary <- df %>%
  mutate(cluster = as.factor(kmeans_result$cluster)) %>%
  group_by(cluster) %>%
  summarise(
    Countries = n(), 
    across(c(child_mort, exports, health, imports, income, 
             inflation, life_expec, total_fer, gdpp),
           ~ round(mean(.), 2))
  )


# here's our summary table for each cluster variables for the k=3
cluster_summary <- df %>%
  mutate(cluster = as.factor(kmeans_result$cluster)) %>%
  group_by(cluster) %>%
  summarise(
    Countries = n(),
    across(c(child_mort, exports, health, imports,  
             inflation, life_expec, total_fer, gdpp),
           ~ round(mean(.), 2))
  )

cluster_summary %>%
  kable(
    format = "html",
    align = c("c", "c", rep("c", 8)),  
    caption = "Cluster Characteristics (k=3)",
    col.names = c("Cluster", "Countries", "Child Mort.", "Exports", "Health",
                  "Imports", "Inflation", "Life Expect.", "Fertility", "GDPP")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    font_size = 13
  ) %>%
  column_spec(1:2, bold = TRUE) %>%
  footnote(
    general = "Mean values for k-means clustering with k=3."
  )



#thus we have cluster1:developed countries, cluster 1: emerging countries, cluster 3: least developed countries

df_kmeans <- df_map %>%
  mutate(
    development_stage = case_when(
      kmeans_result$cluster == 1 ~ "Developed",
      kmeans_result$cluster == 2 ~ "Emerging",
      kmeans_result$cluster == 3 ~ "Least-developed"
    )
  )
world_map <- map_data("world") %>%
  left_join(df_kmeans, by = c("region" = "map_name"))

ggplot(world_map, aes(long, lat, group = group, fill = development_stage)) +
  geom_polygon(color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("Developed" = "#4DAF4A", 
               "Emerging" = "#377EB8", 
               "Least-developed" = "#E41A1C"),
    na.value = "gray90"
  ) +
  coord_fixed(1.3) +
  labs(title = "Countries Development Stages (kmeans, k=3)") +
  theme_void()
#evaluation of kmeans
sil <- silhouette(kmeans_result$cluster, dist(df_scaled))
fviz_silhouette(sil) + 
  labs(title = "Cluster Cohesion/Separation (Silhouette Width)")




#------------kmeans with k=4

set.seed(123)  
k <- 4  
kmeans_result <- kmeans(df_scaled, centers = k, nstart = 25)
df$cluster <- as.factor(kmeans_result$cluster)

#visualizing our clusters
fviz_cluster(
  kmeans_result,
  data = df_scaled,
  palette = RColorBrewer::brewer.pal(4, "Set2"),  
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "top"
    )
) + 
  labs(title = "Country Clusters (K-means, k=4)")  


df %>%
  mutate(cluster = kmeans_result$cluster) %>%
  group_by(cluster) %>%
  summarise(
    median_gdpp = median(gdpp),
    median_child_mort = median(child_mort),
    n_countries = n()
  )



#here's our summary table for each cluster variables for the k=4
cluster_summary <- df %>%
  mutate(cluster = kmeans_result$cluster) %>%
  group_by(cluster) %>%
  summarise(
    Countries = n(),
    across(c(child_mort, exports, health, imports,  
             inflation, life_expec, total_fer, gdpp),
           ~ round(mean(.), 1),
           .names = "{.col}")
  ) %>%
  rename(Cluster = cluster)

cluster_summary %>%
  kable(
    format = "html",
    align = c("c", "c", rep("c", 8)),  
    caption = "Cluster Characteristics (k=4)",
    col.names = c("Cluster", "Countries", 
                  "Child Mort.", "Exports", "Health", "Imports",
                  "Inflation", "Life Expect.", "Fertility", "GDPP")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  column_spec(1, bold = TRUE) %>%
  footnote(
    general = "Mean values for k-means clustering with k=4."
  )

#------------kmeans on pca df
#now let's do kmeans on our reduced pca data:
set.seed(123)
#elbow and dilhouette methods for pca data
fviz_nbclust(df_pca, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(title = "Elbow Method: Optimal k (PCA Data)")

fviz_nbclust(df_pca, kmeans, method = "silhouette") +
  labs(title = "Silhouette Score: Optimal k (PCA Data)")
#kmeans
kmeans_pca <- kmeans(df_pca, centers = 3, nstart = 25)

df$cluster_pca <- as.factor(kmeans_pca$cluster)
#visualizing our clusters
fviz_cluster(
  kmeans_pca,
  data = df_pca,
  palette = RColorBrewer::brewer.pal(3, "Set2"),
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_minimal()
) + 
  labs(title = "K-means Clustering on PCA-Reduced Data")


# t-SNE on our scaled data
set.seed(123)
tsne_out <- Rtsne(df_scaled, perplexity = 30)
df$tsne1 <- tsne_out$Y[,1]
df$tsne2 <- tsne_out$Y[,2]
ggplot(df, aes(tsne1, tsne2, color = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "t-SNE Visualization of Clusters") +
  theme_minimal()

#hierarchical clustering
d <- dist(df_scaled, method = "euclidean")

hc_complete <- hclust(d, method = "complete")
hc_average <- hclust(d, method = "average")
hc_ward <- hclust(d, method = "ward.D2")

#plotting h clusters
par(mfrow = c(1, 3)) 
plot(hc_complete, main = "Complete Linkage", cex = 0.6)
plot(hc_average, main = "Average Linkage", cex = 0.6)
plot(hc_ward, main = "Ward's Method", cex = 0.6)
par(mfrow = c(1, 1)) 

fviz_dend(hc_complete, k = 3, rect = TRUE, main = "Complete Linkage")
fviz_dend(hc_average, k = 3, rect = TRUE, main = "Average Linkage")
fviz_dend(hc_ward, k = 3, rect = TRUE, main = "Ward's Method")

 
clusters_ward <- cutree(hc_ward, k = 3)
df_clusters_ward <- df_scaled
df_clusters_ward$cluster <- factor(clusters_ward)
ward_summary <- aggregate(. ~ cluster, data = df_clusters_ward, mean)
ward_summary_rounded <- ward_summary
ward_summary_rounded[,-1] <- round(ward_summary[,-1], 2)


kable(ward_summary_rounded, 
      caption = "Average Variable Values per Cluster (Ward's Method)", 
      align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

#world map
df_ward <- df_map %>%
  mutate(
    development_stage_ward = case_when(
      clusters_ward == 1 ~ "Least-developed",
      clusters_ward == 2 ~ "Emerging",
      clusters_ward == 3 ~ "Developed"
    )
  )

world_map_ward <- map_data("world") %>%
  left_join(df_ward, by = c("region" = "map_name"))

ggplot(world_map_ward, aes(long, lat, group = group, fill = development_stage_ward)) +
  geom_polygon(color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("Developed" = "#4DAF4A", 
               "Emerging" = "#377EB8", 
               "Least-developed" = "#E41A1C"),
    na.value = "gray90"
  ) +
  coord_fixed(1.3) +
  labs(title = "Global Development Stages (Ward's Method)") +
  theme_void()


# Calculating silhouette width
silhouette_ward <- silhouette(clusters_ward, dist(df_scaled))

fviz_silhouette(silhouette_ward) +
  labs(title = "Silhouette Plot - Ward's Method")

mean_sil_width_ward <- mean(silhouette_ward[, "sil_width"])
cat("Average silhouette width (Ward's method):", round(mean_sil_width_ward, 3), "\n")


#Calculating Cophenetic Correlation Coefficient
coph_ward <- cophenetic(hc_ward)
correlation_cophenetic_ward <- cor(d, coph_ward)
cat("Cophenetic correlation coefficient (Ward's method):", round(correlation_cophenetic_ward, 3), "\n")









