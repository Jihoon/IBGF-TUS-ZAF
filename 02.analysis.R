set.seed(123)
df_3d = df_agg_wide %>% filter(weekend==FALSE) %>% select(Timeper_unpaid, Timeper_paid, Timeper_leisure)

# k-means
km.res <- kmeans(df_3d, 4, nstart = 25)
df_3d$cluster = km.res$cluster

fig = plot_ly(df_3d, type="scatter3d",
              x = ~Timeper_unpaid, y = ~Timeper_paid, z = ~Timeper_leisure,
              color = ~cluster, size=3,
              mode = "markers")  %>%
  add_markers()
fig

# PCA
library(SciViews)
df.pca <- pcomp(~Q116Gender + Q23MaritalStatus + 
                    MonthlyIncomeIndiv + Geo_Type +
                    Q19Train + Q19Bus + Q19Taxi + Q110Shop +
                    Q17FarWater + 
                    Q29Child06HH + 
                    Q31PdWrk +
                    # Q31OwnBusns +
                    Q12WashingMachine + Q12Refrigerator + Q12Car + Q12DishWasher, data = df_agg_wide)

# spatial clustering
library(parallelDist)
similarity_matrix <- exp(-parDist(as.matrix(df_3d))^2 / (2 * 1^2))
similarity_matrix2 <- exp(-dist(as.matrix(df_3d))^2 / (2 * 1^2))

#Compute Eigenvalues and Eigenvectors
eigen_result <- eigen(similarity_matrix)
eigenvalues <- eigen_result$values
eigenvectors <- eigen_result$vectors

#Choose the First k Eigenvectors
k <- 3 
selected_eigenvectors <- eigenvectors[, 1:k]

#Apply K-Means Clustering
cluster_assignments <- kmeans(selected_eigenvectors, centers = k)$cluster