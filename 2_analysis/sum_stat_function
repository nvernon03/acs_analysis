library(dplyr)
library(lazyeval)
library(ggplot2)
library(psych)
library(reshape2)
library(gridExtra)

# FUNCTION TO MAKE BOX PLOT
boxplot_func <- function(data, x_var, y_var, by_var=NULL){
  if (is.null(by_var)){
    plot <- ggplot(data) +
      geom_boxplot(aes_string(y=y_var, x=x_var), 
                   outlier.shape=NA)
  } else {
    data[[by_var]] <- as.factor(data[[by_var]])
    plot <- ggplot(data) +
      geom_boxplot(aes_string(y=y_var, x=x_var, fill=by_var), 
                   outlier.shape=NA)
  }
  
  plot <- plot + 
    theme_minimal()
  
  return(plot)
}

# FUNCTION TO MAKE SUMMARY STAT TABLE
summary_func <- function(data, x_var, y_var, by_var=NULL){
  if (is.null(by_var)){
    grp_data <- data %>% 
      group_by_(x_var)
  } else {
    grp_data <- data %>% 
      group_by_(x_var, by_var)
  }
  
  sum <- grp_data %>%
    filter_(interp(~!is.infinite(v), v=as.name(y_var))) %>%
    filter_(interp(~!is.na(v), v=as.name(y_var))) %>%
    summarise_(mean=interp(~mean(v), v=as.name(y_var)),
              quart_1=interp(~quantile(v, 0.25), v=as.name(y_var)),
              median=interp(~median(v), v=as.name(y_var)),
              quart_3=interp(~quantile(v, 0.75), v=as.name(y_var)),
              min=interp(~min(v), v=as.name(y_var)),
              max=interp(~max(v), v=as.name(y_var)),
              sd=interp(~sd(v), v=as.name(y_var)),
              n="n()")
  
  return(sum)
}

# FUNCTION TO ANALYZE CORRELATION
corr_func <- function(data, vars){
  dt_subset <- data[,..vars] %>% 
    na.omit
  
  dt_subset <- dt_subset[!is.infinite(rowSums(dt_subset)),]
  
  corr_table <- round(cor(dt_subset), 3)
  heat_map <- heatmap_func(corr_table)

  print(corr_table)
  print(heat_map)
}

# FUNCTION TO MAKE CORRELATION HEATMAP
replace_lower_tri <- function(corr_mat){
  corr_mat[lower.tri(corr_mat)] <- NA
  return(corr_mat)
}

heatmap_func <- function(corr_mat){
  upper_tri <- replace_lower_tri(corr_mat)
  melt_corr_mat <- melt(upper_tri, na.rm = TRUE)
  
  ggheatmap <- ggplot(melt_corr_mat, aes(Var1, Var2)) +
    geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value, 2))) +
    scale_fill_gradient(low = "white", high = "red") + 
    coord_fixed() + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.justification = c(1, 0),
          legend.position = c(1, 0.1),
          legend.direction = "horizontal") +
    guides(fill = guide_colorbar(barwidth = 7, barheight = 2,
                                 title.position = "top", title.hjust = 0.5))

  ggheatmap
}

# FUNCTION TO MAKE SCATTER PLOR MATRIX
scatter_func <- function(x_var, y_var, by_var){
  scat <- ggplot(dt[sample(.N, 10000)], aes_string(x=x_var, y=y_var, colour=by_var)) +
    geom_point(alpha=0.1) +  geom_smooth(method=lm) + 
    facet_grid(as.formula(paste(". ~", by_var)))
  
  distr_x <- ggplot(dt, aes_string(x=x_var, colour=by_var)) + 
    geom_density() + theme(legend.position="none")
  
  distr_y <- ggplot(dt, aes_string(x=y_var, colour=by_var)) + 
    geom_density() + theme(legend.position="none")
  
  grid.arrange(scat, 
               arrangeGrob(distr_x,distr_y, ncol=2))
}
