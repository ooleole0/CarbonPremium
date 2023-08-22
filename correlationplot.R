library(stargazer)
library(reshape2)
library(ggplot2)
library(tidyverse)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
}

reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
}

######################for carbon variables###############################
vars <- read.csv("INDEVAR3_SORT.csv")
carb_vars <- vars[, 10:26] %>%
    select(
        scope_1,
        scope_2,
        scope_3,
        scope_1_2,
        scope_1_2_3,
        scope_1_grow,
        scope_1_2_grow,
        scope_1_2_3_grow,
        scope_1_Int,
        scope_1_2_Int,
        scope_1_2_3_Int,
        s1IntSecDev,
        s1_2IntSecDev,
        s1_2_3IntSecDev
    )

######################for carbon betas###############################
# beta_df <- read.csv("betas.csv") %>%
#     select(scope1_LS_beta:s1_2_3IntSecDev_LS_beta)

cor_matrix <- cor(carb_vars, use = "complete.obs")
melted_cormat <- melt(cor_matrix)

upper_tri <- get_upper_tri(cor_matrix)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()

# Reorder the correlation matrix
cormat <- reorder_cormat(cor_matrix)
upper_tri <- get_upper_tri(cor_matrix)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "#6D9EC1", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()+
    labs(x=NULL, y=NULL)
# Print the heatmap
print(ggheatmap)